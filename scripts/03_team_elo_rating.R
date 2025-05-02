# ==================================================================================== #
#                                TEAM ELO SYSTEM IMPLEMENTATION
# ==================================================================================== #

# Clear environment
rm(list = ls())

# ===== Libraries =====
library(dplyr)
library(readr)

# ===== Load Data =====
MasterGameInformation <- readRDS("data/MasterGameInformation.rds")
GameInformation2025 <- readRDS("data/GameInformation2025.rds")
results_scaled <- readRDS("data/results_scaled.rds")

# Load TeamList
teams <- teams_lu_table %>%
  filter(sport.name == "Major League Baseball")

TeamList <- teams %>%
  select(id, name, bref_abbreviation, abbreviation, venue.id, venue.name, league.id, league.name, division.id, division.name)

# ===== Setup Logging =====
log_file <- "logs/script_03_output.log"
cat(format(Sys.time()), "Starting Team Elo System Implementation\n", file = log_file)

# ==================================================================================== #
#                                TEAM ELO SYSTEM with Seasonal Reset
# ==================================================================================== #

# Get best parameters from grid search
best_params <- results_scaled %>% slice(1)
K <- best_params$K
cap <- best_params$cap
decay <- best_params$decay
floor_val <- best_params$floor_val

cat(sprintf("Using optimized parameters:\nK: %d\nCap: %d\nDecay: %.2f\nFloor: %d\n",
            K, cap, decay, floor_val), file = log_file, append = TRUE)

# Initialize Elo ratings
initial_elo <- 1000
Team_Team_team_elos <- TeamList %>%
  select(team_id = id) %>%
  mutate(current_elo = initial_elo)

# Expected result function
Team_expected_result <- function(elo_a, elo_b) {
  1 / (1 + 10 ^ ((elo_b - elo_a) / 400))
}

# ==================================================================================== #
#                           Historical Seasons (2021-2024)
# ==================================================================================== #

TeamEloLog <- list()
previous_season <- NA
valid_game_count <- 0  # Add counter for valid games

for (i in seq_len(nrow(MasterGameInformation))) {
  current_row <- MasterGameInformation[i,]  # Store row in a local variable
  
  # Skip if no score
  if (is.na(current_row$teams.home.score) || is.na(current_row$teams.away.score)) next
  
  valid_game_count <- valid_game_count + 1  # Increment counter only for valid games
  
  current_season <- as.numeric(current_row$season)  # Convert to numeric
  if (!is.na(previous_season) && current_season != previous_season) {
    cat(sprintf("🔄 New season detected (%d), resetting Elo to %d\n", 
                current_season, initial_elo), file = log_file, append = TRUE)
    Team_Team_team_elos$current_elo <- initial_elo
  }
  previous_season <- current_season
  
  home_team <- current_row$teams.home.team.id
  away_team <- current_row$teams.away.team.id
  
  elo_home <- Team_Team_team_elos$current_elo[Team_Team_team_elos$team_id == home_team]
  elo_away <- Team_Team_team_elos$current_elo[Team_Team_team_elos$team_id == away_team]
  
  expected_home <- Team_expected_result(elo_home, elo_away)
  outcome_home <- ifelse(current_row$teams.home.score > current_row$teams.away.score, 1,
                        ifelse(current_row$teams.home.score < current_row$teams.away.score, 0, 0.5))
  
  # Margin of victory multiplier
  margin <- abs(current_row$teams.home.score - current_row$teams.away.score)
  margin_multiplier <- log(margin + 1)
  
  # Base delta calculation
  delta_home <- K * margin_multiplier * (outcome_home - expected_home)
  
  # Win/Loss streak adjustments
  if (valid_game_count >= 5) {
    last_5 <- do.call(rbind, tail(TeamEloLog, 5))
    if (!is.null(last_5)) {
      team_recent <- last_5[last_5$home_team_id == home_team, ]
      if (nrow(team_recent) >= 3) {
        if (sum(team_recent$outcome_home == 1) >= 3) {
          delta_home <- delta_home * 0.8  # Dampen effect of wins
        }
        if (sum(team_recent$outcome_home == 0) >= 3) {
          delta_home <- delta_home * 1.15  # Amplify effect of losses
        }
      }
    }
  }
  
  # Penalize bad losses to weak teams
  if (outcome_home == 0 && elo_away < initial_elo && margin >= 5) {
    delta_home <- delta_home * 1.25
  }
  
  # Dynamic cap based on season progress
  season_progress <- i / nrow(MasterGameInformation)
  dynamic_cap <- max(cap * (1 - decay * season_progress), floor_val)
  
  # Apply cap
  delta_home <- max(min(delta_home, dynamic_cap), -dynamic_cap)
  delta_away <- -delta_home
  
  # Update Elo ratings
  Team_Team_team_elos$current_elo[Team_Team_team_elos$team_id == home_team] <- elo_home + delta_home
  Team_Team_team_elos$current_elo[Team_Team_team_elos$team_id == away_team] <- elo_away + delta_away
  
  # Regression to mean every 15 games
  if (i %% 15 == 0) {
    Team_Team_team_elos$current_elo <- Team_Team_team_elos$current_elo * 0.985 + initial_elo * 0.015
  }
  
  # Log the update
  TeamEloLog[[valid_game_count]] <- data.frame(
    game_pk = current_row$game_pk,
    date = current_row$officialDate,
    home_team_id = home_team,
    away_team_id = away_team,
    home_score = current_row$teams.home.score,
    away_score = current_row$teams.away.score,
    home_elo_before = elo_home,
    away_elo_before = elo_away,
    home_elo_after = elo_home + delta_home,
    away_elo_after = elo_away + delta_away,
    outcome_home = outcome_home,
    expected_home = expected_home,
    elo_delta = delta_home
  )
  
  if (i %% 100 == 0) {
    cat(sprintf("Processed %d of %d games (%.1f%%)\n", 
                i, nrow(MasterGameInformation), 100 * i / nrow(MasterGameInformation)), 
        file = log_file, append = TRUE)
  }
}

# Convert TeamEloLog from list to data frame
TeamEloLog <- bind_rows(TeamEloLog)

# Add team names to logs
TeamEloLog <- TeamEloLog %>%
  left_join(TeamList %>% select(id, home_team_name = name), by = c("home_team_id" = "id")) %>%
  left_join(TeamList %>% select(id, away_team_name = name), by = c("away_team_id" = "id"))

# ==================================================================================== #
#                                2025 Season
# ==================================================================================== #

# Reset Elo ratings for 2025
Team_Team_team_elos_2025 <- TeamList %>%
  select(team_id = id) %>%
  mutate(current_elo = initial_elo)

TeamEloLog2025 <- list()
valid_game_count_2025 <- 0  # Add counter for 2025 valid games

for (i in seq_len(nrow(GameInformation2025))) {
  row <- GameInformation2025[i, ]
  
  # Skip if no score
  if (is.na(row$teams.home.score) || is.na(row$teams.away.score)) next
  
  valid_game_count_2025 <- valid_game_count_2025 + 1  # Increment counter
  
  home_team <- row$teams.home.team.id
  away_team <- row$teams.away.team.id
  
  elo_home <- Team_Team_team_elos_2025$current_elo[Team_Team_team_elos_2025$team_id == home_team]
  elo_away <- Team_Team_team_elos_2025$current_elo[Team_Team_team_elos_2025$team_id == away_team]
  
  expected_home <- Team_expected_result(elo_home, elo_away)
  outcome_home <- ifelse(row$teams.home.score > row$teams.away.score, 1,
                        ifelse(row$teams.home.score < row$teams.away.score, 0, 0.5))
  
  # Margin of victory multiplier
  margin <- abs(row$teams.home.score - row$teams.away.score)
  margin_multiplier <- log(margin + 1)
  
  # Base delta calculation
  delta_home <- K * margin_multiplier * (outcome_home - expected_home)
  
  # Win/Loss streak adjustments
  if (valid_game_count_2025 >= 5) {
    last_5 <- do.call(rbind, tail(TeamEloLog2025, 5))
    if (!is.null(last_5)) {
      team_recent <- last_5[last_5$home_team_id == home_team, ]
      if (nrow(team_recent) >= 3) {
        if (sum(team_recent$outcome_home == 1) >= 3) {
          delta_home <- delta_home * 0.8
        }
        if (sum(team_recent$outcome_home == 0) >= 3) {
          delta_home <- delta_home * 1.15
        }
      }
    }
  }
  
  # Penalize bad losses to weak teams
  if (outcome_home == 0 && elo_away < initial_elo && margin >= 5) {
    delta_home <- delta_home * 1.25
  }
  
  # Dynamic cap based on season progress
  season_progress <- i / nrow(GameInformation2025)
  dynamic_cap <- max(cap * (1 - decay * season_progress), floor_val)
  
  # Apply cap
  delta_home <- max(min(delta_home, dynamic_cap), -dynamic_cap)
  delta_away <- -delta_home
  
  # Update Elo ratings
  Team_Team_team_elos_2025$current_elo[Team_Team_team_elos_2025$team_id == home_team] <- elo_home + delta_home
  Team_Team_team_elos_2025$current_elo[Team_Team_team_elos_2025$team_id == away_team] <- elo_away + delta_away
  
  # Regression to mean every 15 games
  if (i %% 15 == 0) {
    Team_Team_team_elos_2025$current_elo <- Team_Team_team_elos_2025$current_elo * 0.985 + initial_elo * 0.015
  }
  
  # Log the update
  TeamEloLog2025[[valid_game_count_2025]] <- data.frame(
    game_pk = row$game_pk,
    date = row$officialDate,
    home_team_id = home_team,
    away_team_id = away_team,
    home_score = row$teams.home.score,
    away_score = row$teams.away.score,
    home_elo_before = elo_home,
    away_elo_before = elo_away,
    home_elo_after = elo_home + delta_home,
    away_elo_after = elo_away + delta_away,
    outcome_home = outcome_home,
    expected_home = expected_home,
    elo_delta = delta_home
  )
  
  if (i %% 50 == 0) {
    cat(sprintf("2025: Processed %d of %d games (%.1f%%)\n", 
                i, nrow(GameInformation2025), 100 * i / nrow(GameInformation2025)), 
        file = log_file, append = TRUE)
  }
}

TeamEloLog2025 <- bind_rows(TeamEloLog2025)

# Add team names to logs
TeamEloLog2025 <- TeamEloLog2025 %>%
  left_join(TeamList %>% select(id, home_team_name = name), by = c("home_team_id" = "id")) %>%
  left_join(TeamList %>% select(id, away_team_name = name), by = c("away_team_id" = "id"))

# Save outputs
saveRDS(TeamEloLog, "data/TeamEloLog.rds")
saveRDS(TeamEloLog2025, "data/TeamEloLog2025.rds")

# Log completion
cat(format(Sys.time()), "Team Elo System Implementation completed\n", file = log_file, append = TRUE)

# Print summary statistics
cat("\nHistorical Seasons (2021-2024):\n")
cat("Total games processed:", nrow(TeamEloLog), "\n")
cat("Date range:", min(TeamEloLog$date), "to", max(TeamEloLog$date), "\n")
cat("Elo range:", round(min(c(TeamEloLog$home_elo_after, TeamEloLog$away_elo_after))), "to",
    round(max(c(TeamEloLog$home_elo_after, TeamEloLog$away_elo_after))), "\n\n")

cat("2025 Season:\n")
cat("Total games processed:", nrow(TeamEloLog2025), "\n")
cat("Date range:", min(TeamEloLog2025$date), "to", max(TeamEloLog2025$date), "\n")
cat("Elo range:", round(min(c(TeamEloLog2025$home_elo_after, TeamEloLog2025$away_elo_after))), "to",
    round(max(c(TeamEloLog2025$home_elo_after, TeamEloLog2025$away_elo_after))), "\n")




