
# ==================================================================================== #
#                                     PART 1: Training the Model
# ==================================================================================== #

# ==================================================================================== #
#                                     DATA PREPARATION 
# ==================================================================================== #
rm(list = ls())

# ===== Libraries =====
library(baseballr)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(gganimate)
library(scales)
library(tidyr)
library(randomForest)
library(gbm)
library(tibble)

# ================ #
# Team Information
# ================ #
teams <- teams_lu_table %>%
  filter(sport.name == "Major League Baseball")

TeamList <- teams %>%
  select(id, name, bref_abbreviation, abbreviation, venue.id, venue.name, league.id, league.name, division.id, division.name)

team_abbr_lookup <- tibble::tibble(
  team_name = c(
    "Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles", "Boston Red Sox", "Chicago Cubs",
    "Chicago White Sox", "Cincinnati Reds", "Cleveland Guardians", "Colorado Rockies", "Detroit Tigers",
    "Houston Astros", "Kansas City Royals", "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins",
    "Milwaukee Brewers", "Minnesota Twins", "New York Mets", "New York Yankees", "Oakland Athletics",
    "Philadelphia Phillies", "Pittsburgh Pirates", "San Diego Padres", "San Francisco Giants",
    "Seattle Mariners", "St. Louis Cardinals", "Tampa Bay Rays", "Texas Rangers", "Toronto Blue Jays",
    "Washington Nationals"
  ),
  abbreviation = c(
    "ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", 
    "HOU", "KCR", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", 
    "PHI", "PIT", "SDP", "SFG", "SEA", "STL", "TBR", "TEX", "TOR", "WSN"
  )
)

# ====================== #
# SELECT SEASONS TO PULL #
# ====================== #
years <- 2021:2024

# ============================ #
# COLLECT STATS + GAME DATA   #
# ============================ #
for (i in years) {
  cat("🔄 Collecting data for season:", i, "\n")
  
  # --- TEAM STATS ---
  assign(paste0("TeamBattingStatistics", i),
         fg_team_batter(startseason = i, endseason = i, qual = "y"))
  
  assign(paste0("TeamPitchingStatistics", i),
         fg_team_pitcher(startseason = i, endseason = i, qual = "y"))
  
  assign(paste0("TeamFieldingStatistics", i),
         fg_team_fielder(startseason = i, endseason = i, qual = "y"))
  
  # --- GAME INFO ---
  dates <- seq.Date(as.Date(paste0(i, "-03-1")), as.Date(paste0(i, "-11-1")), by = "day")
  dates <- as.character(dates)
  
  game_info_list <- list()
  
  for (date in dates) {
    cat("📅 Fetching games for:", date, "\n")
    
    tryCatch({
      df <- mlb_game_pks(date = date, level_ids = c(1))
      if (is.data.frame(df) && nrow(df) > 0) {
        cat("✅ Games found for", date, ":", nrow(df), "games\n")
        df <- df %>% mutate(game_date = date)
        game_info_list[[date]] <- df
      } else {
        cat("ℹ️ No games found for", date, "\n")
      }
    }, error = function(e) {
      cat("❌ Error on", date, ":", conditionMessage(e), "\n")
    })
  }
  
  game_info <- bind_rows(game_info_list)
  
  GameInformation <- game_info %>%
    select(
      game_pk, season, officialDate, gameNumber, dayNight, gamesInSeries, seriesGameNumber, seriesDescription,
      teams.away.score, teams.away.isWinner, teams.away.seriesNumber, teams.away.leagueRecord.wins,
      teams.away.leagueRecord.losses, teams.away.leagueRecord.pct, teams.away.team.id, teams.away.team.name,
      teams.home.score, teams.home.isWinner, teams.home.seriesNumber, teams.home.leagueRecord.wins,
      teams.home.leagueRecord.losses, teams.home.leagueRecord.pct, teams.home.team.id, teams.home.team.name
    ) %>%
    filter(seriesDescription == "Regular Season") %>%
    left_join(team_abbr_lookup, by = c("teams.home.team.name" = "team_name")) %>%
    rename(home_team_abbr = abbreviation) %>%
    left_join(team_abbr_lookup, by = c("teams.away.team.name" = "team_name")) %>%
    rename(away_team_abbr = abbreviation) %>%
    mutate(Date_Home_Away = paste(officialDate, home_team_abbr, away_team_abbr, sep = "_")) %>%
    filter(!is.na(`teams.away.score`)) %>%
    distinct(game_pk, .keep_all = TRUE) %>%
    distinct(Date_Home_Away, .keep_all = TRUE)
  
  assign(paste0("GameInformation", i), GameInformation)
}

# Combine all years into one master game log
MasterGameInformation <- bind_rows(lapply(years, function(y) {
  get(paste0("GameInformation", y))
}))

cat("📦 Combined total games:", nrow(MasterGameInformation), "\n")


# ==================================================================================== #
#             GRID SEARCH: Optimize K, Max Cap, Decay Rate, Floor using RMSE
# ==================================================================================== #

# Define parameter ranges
K_vals <- c(40, 50, 60)
cap_vals <- c(40, 50, 60)
decay_rates <-c(0.8, 0.9)
floor_vals <- c(5,10)

results <- data.frame()
total_iterations <- length(K_vals) * length(cap_vals) * length(decay_rates) * length(floor_vals)
iteration <- 0

# Loop through all combinations
for (K in K_vals) {
  for (cap in cap_vals) {
    for (decay in decay_rates) {
      for (floor_val in floor_vals) {
        iteration <- iteration + 1
        cat(sprintf("Processing %d of %d: K=%d, cap=%d, decay=%.2f, floor=%d\n", 
                    iteration, total_iterations, K, cap, decay, floor_val))
        
        elo_table <- TeamList %>% select(team_id = id) %>% mutate(current_elo = 1000)
        elo_log <- list()
        previous_season <- NA  # Track season for Elo resets
        
        for (i in 1:nrow(MasterGameInformation)) {
          row <- MasterGameInformation[i, ]
          if (is.na(row$teams.home.score) || is.na(row$teams.away.score)) next
          
          current_season <- row$season
          if (!is.na(previous_season) && current_season != previous_season) {
            elo_table$current_elo <- 1000
          }
          previous_season <- current_season
          
          home <- row$teams.home.team.id
          away <- row$teams.away.team.id
          elo_h <- elo_table$current_elo[elo_table$team_id == home]
          elo_a <- elo_table$current_elo[elo_table$team_id == away]
          
          expected <- 1 / (1 + 10 ^ ((elo_a - elo_h) / 400))
          outcome <- ifelse(row$teams.home.score > row$teams.away.score, 1, 
                            ifelse(row$teams.home.score < row$teams.away.score, 0, 0.5))
          
          margin <- abs(row$teams.home.score - row$teams.away.score)
          margin_multiplier <- log(margin + 1)
          season_progress <- i / nrow(MasterGameInformation)
          dyn_cap <- max(cap * (1 - decay * season_progress), floor_val)
          
          delta <- K * margin_multiplier * (outcome - expected)
          delta <- max(min(delta, dyn_cap), -dyn_cap)
          
          elo_table$current_elo[elo_table$team_id == home] <- elo_h + delta
          elo_table$current_elo[elo_table$team_id == away] <- elo_a - delta
          
          elo_log[[i]] <- data.frame(
            home_team_id = home,
            away_team_id = away,
            home_score = row$teams.home.score,
            away_score = row$teams.away.score,
            home_elo_after = elo_h + delta,
            away_elo_after = elo_a - delta,
            outcome_home = outcome,
            home_elo_before = elo_h,
            away_elo_before = elo_a
          )
        }
        
        elo_df <- bind_rows(elo_log)
        
        final_summary <- elo_df %>%
          transmute(team_id = home_team_id, result = outcome_home, elo_after = home_elo_after) %>%
          bind_rows(
            elo_df %>% transmute(team_id = away_team_id, result = 1 - outcome_home, elo_after = away_elo_after)
          ) %>%
          group_by(team_id) %>%
          summarise(
            win_pct = mean(result),
            final_elo = last(elo_after),
            .groups = "drop"
          ) %>%
          left_join(TeamList %>% select(team_id = id, team_name = name), by = "team_id")
        
        # Correlation with opponent Elo strength
        opp_strength <- elo_df %>%
          transmute(team_id = home_team_id, opponent_elo = away_elo_before) %>%
          bind_rows(
            elo_df %>% transmute(team_id = away_team_id, opponent_elo = home_elo_before)
          ) %>%
          group_by(team_id) %>%
          summarise(avg_opp_elo = mean(opponent_elo), .groups = "drop")
        
        final_summary <- final_summary %>% left_join(opp_strength, by = "team_id")
        
        model <- lm(final_elo ~ win_pct, data = final_summary)
        rmse <- sqrt(mean((final_summary$final_elo - predict(model))^2))
        r2 <- summary(model)$r.squared
        opp_corr <- cor(final_summary$final_elo, final_summary$avg_opp_elo)
        
        results <- bind_rows(results, data.frame(K, cap, decay, floor_val, rmse, r2, opp_corr))
      }
    }
  }
}

# Normalize RMSE and Opponent Correlation
results_scaled <- results %>%
  mutate(
    rmse_scaled = (rmse - min(rmse)) / (max(rmse) - min(rmse)),
    opp_scaled = (abs(opp_corr) - min(abs(opp_corr))) / (max(abs(opp_corr)) - min(abs(opp_corr)))
  ) %>%
  mutate(
    composite_score = 0.6 * (1 - rmse_scaled) + 0.4 * opp_scaled
  ) %>%
  arrange(desc(composite_score))

View(results_scaled)

# Plot
TeamEloParameterPlot = ggplot(results, aes(x = r2, y = rmse, label = K)) +
  geom_point(aes(color = as.factor(K)), size = 3) +
  geom_text(vjust = 1.5, size = 3) +
  labs(title = "RMSE vs R²: Grid Search Results",
       x = "R-squared (final_elo ~ win_pct)",
       y = "RMSE",
       color = "K value") +
  theme_minimal()

TeamEloParameterPlot


# ==================================================================================== #
#                                TEAM ELO SYSTEM with Seasonal Reset
# ==================================================================================== #

initial_elo <- 1000
K <- results_scaled$K
Team_Team_team_elos <- TeamList %>%
  select(team_id = id) %>%
  mutate(current_elo = initial_elo)

Team_expected_result <- function(elo_a, elo_b) {
  1 / (1 + 10 ^ ((elo_b - elo_a) / 400))
}

TeamEloLog <- list()
previous_season <- NA  # initialize to track season transitions

for (i in 1:nrow(MasterGameInformation)) {
  row <- MasterGameInformation[i, ]
  
  # Skip if no score
  if (is.na(row$teams.home.score) || is.na(row$teams.away.score)) next
  
  current_season <- row$season
  if (!is.na(previous_season) && current_season != previous_season) {
    cat("🔁 New season detected (", current_season, "), resetting Elo to 1000\n")
    Team_Team_team_elos$current_elo <- initial_elo
  }
  previous_season <- current_season
  
  home_team <- row$teams.home.team.id
  away_team <- row$teams.away.team.id
  
  elo_home <- Team_Team_team_elos$current_elo[Team_Team_team_elos$team_id == home_team]
  elo_away <- Team_Team_team_elos$current_elo[Team_Team_team_elos$team_id == away_team]
  
  expected_home <- Team_expected_result(elo_home, elo_away)
  outcome_home <- ifelse(row$teams.home.score > row$teams.away.score, 1,
                         ifelse(row$teams.home.score < row$teams.away.score, 0, 0.5))
  
  margin <- abs(row$teams.home.score - row$teams.away.score)
  margin_multiplier <- log(margin + 1)
  
  season_progress <- i / nrow(MasterGameInformation)
  dynamic_cap <- max(results_scaled$cap * (1 - results_scaled$decay * season_progress), results_scaled$floor_val)
  
  delta_home <- K * margin_multiplier * (outcome_home - expected_home)
  
  # Win/Loss streak adjustments
  if (i >= 5) {
    last_5 <- do.call(rbind, tail(TeamEloLog, 5))
    team_recent <- last_5 %>% filter(home_team_id == home_team)
    if (nrow(team_recent) >= 3 && sum(team_recent$outcome_home == 1) >= 3) {
      delta_home <- delta_home * 0.8
    }
    if (nrow(team_recent) >= 3 && sum(team_recent$outcome_home == 0) >= 3) {
      delta_home <- delta_home * 1.15
    }
  }
  
  # Penalize bad losses to weak teams
  if (outcome_home == 0 && elo_away < 1000 && margin >= 5) {
    delta_home <- delta_home * 1.25
  }
  
  # Apply cap
  delta_home <- max(min(delta_home, dynamic_cap), -dynamic_cap)
  delta_away <- -delta_home
  
  # Update Elo
  Team_Team_team_elos$current_elo[Team_Team_team_elos$team_id == home_team] <- elo_home + delta_home
  Team_Team_team_elos$current_elo[Team_Team_team_elos$team_id == away_team] <- elo_away + delta_away
  
  # Regression to mean every 15 games
  if (i %% 15 == 0) {
    Team_Team_team_elos$current_elo <- Team_Team_team_elos$current_elo * 0.985 + initial_elo * 0.015
  }
  
  TeamEloLog[[i]] <- data.frame(
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
}

TeamEloLog <- bind_rows(TeamEloLog)

# Average Opponent Elo
opponent_strength <- TeamEloLog %>%
  transmute(team_id = home_team_id, opponent_elo = away_elo_before) %>%
  bind_rows(
    TeamEloLog %>%
      transmute(team_id = away_team_id, opponent_elo = home_elo_before)
  ) %>%
  group_by(team_id) %>%
  summarise(avg_opp_elo = mean(opponent_elo), .groups = "drop")

# ==================================================================================== #
#                        ELO + WIN RATIO PLOT FOR INDIVIDUAL TEAM
# ==================================================================================== #

Team_plot_id <- 112   # e.g. Yankees
Team_plot_name <- TeamList %>% filter(id == Team_plot_id) %>% pull(name)

TeamEloHistory <- TeamEloLog %>%
  filter(home_team_id == Team_plot_id | away_team_id == Team_plot_id) %>%
  mutate(
    Team_elo = ifelse(home_team_id == Team_plot_id, home_elo_after, away_elo_after),
    opponent_id = ifelse(home_team_id == Team_plot_id, away_team_id, home_team_id),
    team_score = ifelse(home_team_id == Team_plot_id, home_score, away_score),
    opponent_score = ifelse(home_team_id == Team_plot_id, away_score, home_score),
    score_label = paste0(team_score, "–", opponent_score),
    outcome = case_when(
      team_score > opponent_score ~ "Win",
      team_score < opponent_score ~ "Loss",
      TRUE ~ "Tie"
    ),
    season = lubridate::year(as.Date(date)),  # extract season from game date
    win = ifelse(outcome == "Win", 1, 0),
    loss = ifelse(outcome == "Loss", 1, 0)
  ) %>%
  group_by(season) %>%
  mutate(
    cum_win = cumsum(win),
    cum_loss = cumsum(loss),
    win_ratio = cum_win / (cum_win + cum_loss),
    win_ratio_scaled = 500 + win_ratio * 1000 
  ) %>%
  ungroup() %>%
  left_join(TeamList %>% select(id, opponent_abbr = abbreviation), by = c("opponent_id" = "id")) %>%
  arrange(as.Date(date)) %>%
  mutate(frame = row_number())

# === Plot Final Point
final_point <- TeamEloHistory %>%
  arrange(as.Date(date)) %>%
  slice_tail(n = 1)

# === Static Plot: Elo & Win Ratio
ggplot(TeamEloHistory, aes(x = as.Date(date))) +
  geom_line(aes(y = Team_elo), color = "#1f1f1f", size = 1.1) +
  geom_line(aes(y = win_ratio_scaled), color = "#1b9e77", linetype = "dashed", size = 1) +
  geom_text(data = final_point,
            aes(y = Team_elo, label = paste0("Elo: ", round(Team_elo))),
            hjust = 1.1, vjust = -5.1, color = "#1f1f1f", fontface = "bold") +
  geom_text(data = final_point,
            aes(y = win_ratio_scaled, label = paste0("Win Ratio: ", scales::percent(win_ratio))),
            hjust = 1.1, vjust = -8.1, color = "#1b9e77", fontface = "bold") +
  scale_y_continuous(
    name = "Elo Rating",
    limits = c(500, 1500),
    sec.axis = sec_axis(~ (. - 500) / 1000, name = "Win Ratio", labels = percent_format())
  ) +
  scale_x_date(expand = expansion(mult = c(0.01, 0.05))) +
  labs(
    title = paste("📈 Elo & Win Ratio over Time for", Team_plot_name),
    subtitle = "Solid = Elo | Dashed = Win Ratio",
    x = "Date", y = "Elo Rating"
  ) +
  theme_minimal(base_size = 14)

# === Animated Elo Plot with Labels
Team_line_history <- do.call(rbind, lapply(1:nrow(TeamEloHistory), function(i) {
  TeamEloHistory[1:i, ] %>% mutate(frame = i)
}))

Team_label_history <- Team_line_history  # same structure

Team_plot <- ggplot() +
  geom_line(data = Team_line_history, aes(x = as.Date(date), y = Team_elo, group = 1), color = "#1f1f1f", size = 1.1) +
  geom_point(data = Team_line_history, aes(x = as.Date(date), y = Team_elo), color = "#1f1f1f", size = 2) +
  geom_label_repel(
    data = Team_label_history,
    aes(x = as.Date(date), y = Team_elo, label = paste(opponent_abbr, score_label), fill = outcome, group = interaction(date, opponent_abbr)),
    size = 3.2,
    color = "white",
    fontface = "bold",
    show.legend = FALSE,
    max.overlaps = 50,
    seed = 42
  ) +
  scale_fill_manual(values = c("Win" = "#1b9e77", "Loss" = "#d95f02", "Tie" = "gray50")) +
  labs(
    title = paste("📈 Elo Trajectory for", Team_plot_name),
    subtitle = "Line shows Elo change | Labels accumulate with score and result",
    x = "Date", y = "Elo Rating", fill = "Game Outcome"
  ) +
  theme_minimal(base_size = 14) +
  transition_manual(frame)

# ==================================================================================== #
#                         FINAL TEAM SUMMARY TABLE FOR RANKING
# ==================================================================================== #

TeamFinalSummary <- TeamEloLog %>%
  transmute(team_id = home_team_id, result = outcome_home, elo_after = home_elo_after) %>%
  bind_rows(
    TeamEloLog %>% transmute(team_id = away_team_id, result = 1 - outcome_home, elo_after = away_elo_after)
  ) %>%
  group_by(team_id) %>%
  summarise(
    games_played = n(),
    wins = sum(result == 1),
    losses = sum(result == 0),
    win_pct = round(wins / games_played, 3),
    final_elo = last(elo_after),
    .groups = "drop"
  ) %>%
  left_join(TeamList %>% select(team_id = id, team_name = name), by = "team_id") %>%
  left_join(opponent_strength, by = "team_id") %>%
  select(team_id, team_name, games_played, wins, losses, win_pct, final_elo, avg_opp_elo) %>%
  arrange(desc(final_elo))

View(TeamFinalSummary)

# Final Elo vs Win Percentage 
ggplot(TeamFinalSummary, aes(x = win_pct, y = final_elo)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  geom_text(aes(label = team_name), vjust = -1.1, size = 3) +
  labs(title = "Final Elo vs Win Percentage",
       x = "Win Percentage",
       y = "Final Elo") +
  theme_minimal(base_size = 14)

# Fit linear model
elo_model <- lm(final_elo ~ win_pct, data = TeamFinalSummary)

# Calculate RMSE
rmse <- sqrt(mean((TeamFinalSummary$final_elo - predict(elo_model))^2))
print(paste("RMSE:", round(rmse, 2)))

# Get R-squared
r_squared <- summary(elo_model)$r.squared
print(paste("R-squared:", round(r_squared, 4)))

# ==================================================================================== #
#                             PREDICTING RUNS SCORED (Multi-Season)
# ==================================================================================== #

# Team name abbreviation mapping
fielding_map <- c(
  "Diamondbacks" = "ARI", "Braves" = "ATL", "Orioles" = "BAL", "Red Sox" = "BOS", "Cubs" = "CHC",
  "White Sox" = "CHW", "Reds" = "CIN", "Guardians" = "CLE", "Rockies" = "COL", "Tigers" = "DET",
  "Astros" = "HOU", "Royals" = "KCR", "Angels" = "LAA", "Dodgers" = "LAD", "Marlins" = "MIA",
  "Brewers" = "MIL", "Twins" = "MIN", "Mets" = "NYM", "Yankees" = "NYY", "Athletics" = "OAK",
  "Phillies" = "PHI", "Pirates" = "PIT", "Padres" = "SDP", "Giants" = "SFG", "Mariners" = "SEA",
  "Cardinals" = "STL", "Rays" = "TBR", "Rangers" = "TEX", "Blue Jays" = "TOR", "Nationals" = "WSN"
)

# Helper to merge stats
merge_stats <- function(data, stats, team_col, prefix) {
  stats_renamed <- stats %>%
    rename_with(~ paste0(prefix, "_", .x), -team_name)
  data %>%
    left_join(stats_renamed, by = setNames("team_name", team_col))
}

# Build per-season GameData list
all_GameData <- list()

for (year in years) {
  cat("📦 Merging team stats for season:", year, "\n")
  
  games_year <- MasterGameInformation %>%
    filter(season == year) %>%
    rename(
      home_runs = teams.home.score,
      away_runs = teams.away.score
    )
  
  # Season-specific team stats
  batting_stats <- get(paste0("TeamBattingStatistics", year)) %>%
    mutate(
      wRC = wRC / G,
      R = R / G
    ) %>%
    select(team_name, OBP, SLG, AVG, wRC, R)
  
  pitching_stats <- get(paste0("TeamPitchingStatistics", year)) %>%
    select(team_name, ERA, WHIP, K_9, BB_9, HR_9)
  
  fielding_stats <- get(paste0("TeamFieldingStatistics", year)) %>%
    mutate(
      team_name = fielding_map[team_name],
      DRS = DRS / G,
      Defense = Defense / G
    ) %>%
    filter(!is.na(team_name)) %>%
    select(team_name, DRS, Defense)
  
  
  # Merge Elo
  games_year <- games_year %>%
    left_join(TeamEloLog %>% select(game_pk, home_elo_before, away_elo_before), by = "game_pk")
  
  # Merge stats for both teams
  games_year <- games_year %>%
    merge_stats(batting_stats, "home_team_abbr", "home") %>%
    merge_stats(pitching_stats, "home_team_abbr", "home") %>%
    merge_stats(fielding_stats, "home_team_abbr", "home") %>%
    merge_stats(batting_stats, "away_team_abbr", "away") %>%
    merge_stats(pitching_stats, "away_team_abbr", "away") %>%
    merge_stats(fielding_stats, "away_team_abbr", "away")
  
  all_GameData[[as.character(year)]] <- games_year
}

# Combine all years into one game-level data frame
GameData <- bind_rows(all_GameData)

# Final dataset for modeling
TrainingData <- GameData %>%
  select(
    game_pk, officialDate, season, home_team_abbr, away_team_abbr,
    home_elo_before, away_elo_before, home_runs, away_runs,
    starts_with("home_"), starts_with("away_")
  )

# Split into train/test
set.seed(42)
TrainingDataClean <- TrainingData %>% na.omit()
rows <- nrow(TrainingDataClean)
train_indices <- sample(1:rows, size = 0.8 * rows)

train_data <- TrainingDataClean[train_indices, ]
test_data  <- TrainingDataClean[-train_indices, ]

# ======================= #
# LINEAR REGRESSION MODEL 
# ======================= #

# === HOME TEAM === #
lm_home <- lm(home_runs ~ home_elo_before + away_elo_before +
                home_OBP + home_SLG + home_AVG + home_wRC + home_R +
                home_ERA + home_WHIP + home_K_9 + home_BB_9 + home_HR_9 +
                home_DRS + home_Defense +
                away_ERA + away_WHIP + away_K_9 + away_BB_9 + away_HR_9 +
                away_DRS + away_Defense,
              data = train_data)

home_preds_lm <- predict(lm_home, newdata = test_data)
home_rmse_lm <- sqrt(mean((test_data$home_runs - home_preds_lm)^2))
cat("📊 Home Team RMSE:", round(home_rmse_lm, 3), "\n")

# === AWAY TEAM === #
lm_away <- lm(away_runs ~ away_elo_before + home_elo_before +
                away_OBP + away_SLG + away_AVG + away_wRC + away_R +
                away_ERA + away_WHIP + away_K_9 + away_BB_9 + away_HR_9 +
                away_DRS + away_Defense +
                home_ERA + home_WHIP + home_K_9 + home_BB_9 + home_HR_9 +
                home_DRS + home_Defense,
              data = train_data)

away_preds_lm <- predict(lm_away, newdata = test_data)
away_rmse_lm <- sqrt(mean((test_data$away_runs - away_preds_lm)^2))
cat("📊 Away Team RMSE:", round(away_rmse_lm, 3), "\n")

# ========== #
# GBM MODEL
# ========= #

# === HOME TEAM === #
gbm_home <- gbm(
  formula = home_runs ~ home_elo_before + away_elo_before +
    home_OBP + home_SLG + home_AVG + home_wRC + home_R +
    home_ERA + home_WHIP + home_K_9 + home_BB_9 + home_HR_9 +
    home_DRS + home_Defense +
    away_ERA + away_WHIP + away_K_9 + away_BB_9 + away_HR_9 +
    away_DRS + away_Defense,
  data = train_data,
  distribution = "gaussian",
  n.trees = 3000,
  interaction.depth = 4,
  shrinkage = 0.01,
  cv.folds = 5,
  verbose = FALSE
)

best_iter_home <- gbm.perf(gbm_home, method = "cv")
home_preds_gbm <- predict(gbm_home, newdata = test_data, n.trees = best_iter_home)
home_rmse_gbm <- sqrt(mean((test_data$home_runs - home_preds_gbm)^2))
cat("📈 GBM Home Team RMSE:", round(home_rmse_gbm, 3), "\n")

# === AWAY TEAM === #
gbm_away <- gbm(
  formula = away_runs ~ away_elo_before + home_elo_before +
    away_OBP + away_SLG + away_AVG + away_wRC + away_R +
    away_ERA + away_WHIP + away_K_9 + away_BB_9 + away_HR_9 +
    away_DRS + away_Defense +
    home_ERA + home_WHIP + home_K_9 + home_BB_9 + home_HR_9 +
    home_DRS + home_Defense,
  data = train_data,
  distribution = "gaussian",
  n.trees = 3000,
  interaction.depth = 4,
  shrinkage = 0.01,
  cv.folds = 5,
  verbose = FALSE
)

best_iter_away <- gbm.perf(gbm_away, method = "cv")
away_preds_gbm <- predict(gbm_away, newdata = test_data, n.trees = best_iter_away)
away_rmse_gbm <- sqrt(mean((test_data$away_runs - away_preds_gbm)^2))
cat("📈 GBM Away Team RMSE:", round(away_rmse_gbm, 3), "\n")

# ============= #
# POISSON MODEL
# ============= #

# === HOME TEAM === #
glm_home <- glm(home_runs ~ home_elo_before + away_elo_before +
                  home_OBP + home_SLG + home_AVG + home_wRC + home_R +
                  home_ERA + home_WHIP + home_K_9 + home_BB_9 + home_HR_9 +
                  away_ERA + away_WHIP + away_K_9 + away_BB_9 + away_HR_9,
                family = "poisson", data = train_data)

home_preds_poisson <- predict(glm_home, newdata = test_data, type = "response")
home_rmse_poisson <- sqrt(mean((test_data$home_runs - home_preds_poisson)^2))
cat("📦 Poisson RMSE - Home:", round(home_rmse_poisson, 3), "\n")

# === AWAY TEAM === #
glm_away <- glm(away_runs ~ away_elo_before + home_elo_before +
                  away_OBP + away_SLG + away_AVG + away_wRC + away_R +
                  away_ERA + away_WHIP + away_K_9 + away_BB_9 + away_HR_9 +
                  home_ERA + home_WHIP + home_K_9 + home_BB_9 + home_HR_9,
                family = "poisson", data = train_data)

away_preds_poisson <- predict(glm_away, newdata = test_data, type = "response")
away_rmse_poisson <- sqrt(mean((test_data$away_runs - away_preds_poisson)^2))
cat("📦 Poisson RMSE - Away:", round(away_rmse_poisson, 3), "\n")

# =================== #
# RANDOM FOREST MODEL
# =================== #

# === HOME TEAM === #
rf_home <- randomForest(
  home_runs ~ home_elo_before + away_elo_before +
    home_OBP + home_SLG + home_AVG + home_wRC + home_R +
    home_ERA + home_WHIP + home_K_9 + home_BB_9 + home_HR_9 +
    away_ERA + away_WHIP + away_K_9 + away_BB_9 + away_HR_9,
  data = train_data,
  ntree = 500
)

rf_preds_home <- predict(rf_home, newdata = test_data)
rf_rmse_home <- sqrt(mean((test_data$home_runs - rf_preds_home)^2))
cat("🌲 Random Forest RMSE - Home:", round(rf_rmse_home, 3), "\n")

# === AWAY TEAM === #
rf_away <- randomForest(
  away_runs ~ away_elo_before + home_elo_before +
    away_OBP + away_SLG + away_AVG + away_wRC + away_R +
    away_ERA + away_WHIP + away_K_9 + away_BB_9 + away_HR_9 +
    home_ERA + home_WHIP + home_K_9 + home_BB_9 + home_HR_9,
  data = train_data,
  ntree = 500
)

rf_preds_away <- predict(rf_away, newdata = test_data)
rf_rmse_away <- sqrt(mean((test_data$away_runs - rf_preds_away)^2))
cat("🌲 Random Forest RMSE - Away:", round(rf_rmse_away, 3), "\n")

# ===================================== #
# CHOOSING BEST MODEL & PREDICTING RUNS
# ===================================== #

# === COMPARE MODELS AND SELECT BEST BY RMSE === #
model_rmse <- c(
  poisson_home = home_rmse_poisson,
  poisson_away = away_rmse_poisson,
  linear_home = home_rmse_lm,
  linear_away = away_rmse_lm,
  gbm_home = home_rmse_gbm,
  gbm_away = away_rmse_gbm
)

home_models <- model_rmse[c("poisson_home", "linear_home", "gbm_home")]
away_models <- model_rmse[c("poisson_away", "linear_away", "gbm_away")]

best_home_model <- names(home_models)[which.min(home_models)]
best_away_model <- names(away_models)[which.min(away_models)]

# === GENERATE PREDICTIONS === #
TrainingDataClean <- TrainingDataClean %>%
  mutate(
    predicted_home_runs = case_when(
      best_home_model == "poisson_home" ~ predict(glm_home, newdata = TrainingDataClean, type = "response"),
      best_home_model == "linear_home" ~ predict(lm_home, newdata = TrainingDataClean),
      best_home_model == "gbm_home" ~ predict(gbm_home, newdata = TrainingDataClean, n.trees = best_iter_home)
    ),
    
    predicted_away_runs = case_when(
      best_away_model == "poisson_away" ~ predict(glm_away, newdata = TrainingDataClean, type = "response"),
      best_away_model == "linear_away" ~ predict(lm_away, newdata = TrainingDataClean),
      best_away_model == "gbm_away" ~ predict(gbm_away, newdata = TrainingDataClean, n.trees = best_iter_away)
    )
  )

# === ATTACH TO TeamEloLog USING game_pk === #
predictions <- TrainingDataClean %>%
  select(game_pk, predicted_home_runs, predicted_away_runs)

TeamEloLog <- TeamEloLog %>%
  left_join(predictions, by = "game_pk")

# ==================================================================================== #
#                           PART 2: Estimating using 2025 Data
# ==================================================================================== #

# ====================== #
# IMPORTANT: SELECT YEAR
# ====================== #
target_year <- 2025

# ================= #
# 2025 Team Rosters
# ================= #
teams_roster_2025 <- data.frame()

for (i in 1:nrow(TeamList)) {
  team_id <- TeamList$id[i]
  team_name <- TeamList$name[i]
  team_abbreviation <- TeamList$abbreviation[i]
  
  cat("Fetching roster for:", team_name, "(", team_abbreviation, ")\n")
  
  tryCatch({
    roster <- mlb_rosters(team_id = team_id, season = target_year, roster_type = "fullRoster")
    
    roster$team_id <- team_id
    roster$team_name <- team_name
    roster$team_abbreviation <- team_abbreviation
    
    teams_roster_2025 <- bind_rows(teams_roster_2025, roster)
    
    cat("✅ Successfully added roster for", team_name, "\n")
  }, error = function(e) {
    cat("❌ Error retrieving roster for", team_name, ":", conditionMessage(e), "\n")
  })
}

Rosters2025 <- teams_roster_2025 %>%
  distinct(person_id, .keep_all = TRUE) %>%
  select(
    season, team_id, team_abbreviation, team_name, 
    person_id, person_full_name, position_name, position_abbreviation, status_description
  )

# ===================== #
# Creating a Unified PK #
# ===================== #
chadwick_raw <- chadwick_player_lu() 
chadwick_lookup <- chadwick_raw %>% filter(!is.na(key_fangraphs)) %>% select(mlb_id = key_mlbam, fangraphs_id = key_fangraphs)

Rosters2025 <- Rosters2025 %>%
  left_join(chadwick_lookup, by = c("person_id" = "mlb_id")) %>%
  mutate(player_pk = ifelse(!is.na(fangraphs_id), fangraphs_id, person_id)) %>%
  select(season, team_id, team_abbreviation, team_name, person_id, person_full_name, position_name, position_abbreviation, status_description, player_pk)

# ======================= #
# Team Statistics in 2025 #
# ======================= #
TeamFieldingStatistics2025 <- fg_team_fielder(startseason = target_year, endseason = target_year, qual = 'y')
TeamBattingStatistics2025 <- fg_team_batter(startseason = target_year, endseason = target_year, qual = 'y')
TeamPitchingStatistics2025 <- fg_team_pitcher(startseason = target_year, endseason = target_year, qual = 'y')

# ========================= #
# Player Statistics in 2025 #
# ========================= #
PlayerFieldingStatistics2025 <- fg_fielder_leaders(startseason = target_year, endseason = target_year)
PlayerBattingStatistics2025 <- fg_batter_leaders(startseason = target_year, endseason = target_year)

# ======================== #
# Game Information in 2025 #
# ======================== #
dates <- seq.Date(as.Date("2025-03-01"), as.Date("2025-11-1"), by = "day")
dates <- as.character(dates)

game_info_list_2025 <- list()
instance_count <- 0
total_iterations <- length(dates)

for (date in dates) {
  instance_count <- instance_count + 1
  cat("[", instance_count, "/", total_iterations, "] Fetching games for date:", date, "\n")
  
  tryCatch({
    df <- mlb_game_pks(date = date, level_ids = c(1))
    if (is.data.frame(df) && nrow(df) > 0) {
      df <- df %>% mutate(game_date = date)
      game_info_list_2025[[date]] <- df
      cat("✅ Data retrieved for", date, "- Rows:", nrow(df), "\n")
    } else {
      cat("⚠️ No games found for", date, "\n")
    }
  }, error = function(e) {
    cat("❌ Error fetching data for", date, ":", conditionMessage(e), "\n")
  })
}

game_info_2025 <- bind_rows(game_info_list_2025)

GameInformation2025 <- game_info_2025 %>%
  select(
    game_pk, season, officialDate, gameNumber, dayNight, gamesInSeries, seriesGameNumber, seriesDescription,
    teams.away.score, teams.away.isWinner, teams.away.seriesNumber, teams.away.leagueRecord.wins,
    teams.away.leagueRecord.losses, teams.away.leagueRecord.pct, teams.away.team.id, teams.away.team.name,
    teams.home.score, teams.home.isWinner, teams.home.seriesNumber, teams.home.leagueRecord.wins,
    teams.home.leagueRecord.losses, teams.home.leagueRecord.pct, teams.home.team.id, teams.home.team.name
  ) %>%
  filter(seriesDescription == "Regular Season") %>%
  left_join(team_abbr_lookup, by = c("teams.home.team.name" = "team_name")) %>%
  rename(home_team_abbr = abbreviation) %>%
  left_join(team_abbr_lookup, by = c("teams.away.team.name" = "team_name")) %>%
  rename(away_team_abbr = abbreviation) %>%
  mutate(Date_Home_Away = paste(officialDate, home_team_abbr, away_team_abbr, sep = "_")) %>%
  filter(!is.na(teams.away.score)) %>%
  distinct(game_pk, .keep_all = TRUE) %>%
  distinct(Date_Home_Away, .keep_all = TRUE)

cat("\n✅ Scraping complete. Collected game data for all of 2025.\n")

# ============================= #
# Batting & Pitching Game Logs
# ============================= #

baseball_data_2025 <- list()

all_players <- chadwick_player_lu() %>%
  filter(!is.na(key_fangraphs), mlb_played_last == target_year) %>%
  select(player_id = key_fangraphs)

years <- target_year:target_year

scrape_batter_game_logs_2025 <- function() {
  logs <- list()
  instance_count <- 0
  total_iterations <- length(years) * nrow(all_players)
  
  for (year in years) {
    cat("\n=== Scraping batter logs for year:", year, "===\n")
    for (player in all_players$player_id) {
      instance_count <- instance_count + 1
      cat("[", instance_count, "/", total_iterations, "] PlayerID:", player, "\n")
      tryCatch({
        df <- fg_batter_game_logs(playerid = player, year = year)
        if (is.data.frame(df) && nrow(df) > 0) {
          df <- df %>% mutate(player_id = player, year = year)
          logs[[paste0(player, "_", year)]] <- df
        }
      }, error = function(e) {
        cat("❌ Error for PlayerID:", player, ":", conditionMessage(e), "\n")
      })
    }
  }
  bind_rows(logs)
}

scrape_pitcher_game_logs_2025 <- function() {
  logs <- list()
  instance_count <- 0
  total_iterations <- length(years) * nrow(all_players)
  
  for (year in years) {
    cat("\n=== Scraping pitcher logs for year:", year, "===\n")
    for (player in all_players$player_id) {
      instance_count <- instance_count + 1
      cat("[", instance_count, "/", total_iterations, "] PlayerID:", player, "\n")
      tryCatch({
        df <- fg_pitcher_game_logs(playerid = player, year = year)
        if (is.data.frame(df) && nrow(df) > 0) {
          df <- df %>% mutate(player_id = player, year = year)
          logs[[paste0(player, "_", year)]] <- df
        }
      }, error = function(e) {
        cat("❌ Error for PlayerID:", player, ":", conditionMessage(e), "\n")
      })
    }
  }
  bind_rows(logs)
}

baseball_data_2025$batter_game_logs <- scrape_batter_game_logs_2025()
baseball_data_2025$pitcher_game_logs <- scrape_pitcher_game_logs_2025()

# Post-process logs
BatterGameLogs2025 <- baseball_data_2025$batter_game_logs %>%
  filter(AB > 0) %>%
  mutate(
    Home_Away = ifelse(startsWith(Opp, "@"), "Away", "Home"),
    HomeTeam = ifelse(startsWith(Opp, "@"), substr(Opp, 2, 4), Team),
    AwayTeam = ifelse(startsWith(Opp, "@"), Team, Opp),
    Date_Home_Away = paste(Date, HomeTeam, AwayTeam, sep = "_")
  ) %>%
  left_join(GameInformation2025 %>% select(Date_Home_Away, game_pk), by = "Date_Home_Away")

PitcherGameLogs2025 <- baseball_data_2025$pitcher_game_logs %>%
  mutate(
    HomeTeam = ifelse(startsWith(Opp, "@"), substr(Opp, 2, 4), Team),
    AwayTeam = ifelse(startsWith(Opp, "@"), Team, Opp),
    Date_Home_Away = paste(Date, HomeTeam, AwayTeam, sep = "_")
  ) %>%
  left_join(GameInformation2025 %>% select(Date_Home_Away, game_pk), by = "Date_Home_Away")

# Unified PK
BatterGameLogs2025 <- BatterGameLogs2025 %>% rename(player_pk = playerid)
PitcherGameLogs2025 <- PitcherGameLogs2025 %>% rename(player_pk = playerid)
PlayerBattingStatistics2025 <- PlayerBattingStatistics2025 %>% rename(player_pk = playerid)
PlayerFieldingStatistics2025 <- PlayerFieldingStatistics2025 %>% rename(player_pk = playerid)

print("✅ Unified primary key created and applied for 2025 datasets.")

# ==================================================================================== #
#                      PLAYER ELO SYSTEM — 2025 (WIP: NEED TO TRAIN)
# ==================================================================================== #

# === SETUP === #

# 1. Initialize Elo Ratings for All Batters
Player_batter_elos <- Rosters2025 %>%
  distinct(player_pk) %>%
  mutate(current_elo = 1000)

# 2. Identify Starting Pitchers
starting_pitchers <- PitcherGameLogs2025 %>%
  filter(GS == 1) %>%
  select(game_pk, pitcher_team = Team, pitcher_pk = player_pk)

# 3. Join Batters with Starting Pitchers
Player_BatterWithPitcher <- BatterGameLogs2025 %>%
  select(game_pk, batter_pk = player_pk, Team, Date, `1B`, `2B`, `3B`, HR, BB, IBB, HBP, AB) %>%
  mutate(across(c(`1B`, `2B`, `3B`, HR, BB, IBB, HBP, AB), as.numeric)) %>%
  mutate(
    UBB = BB - IBB,
    TotalBases = `1B` + 2 * `2B` + 3 * `3B` + 4 * HR,
    OnBaseEvents = UBB + HBP,
    GamePerformance = TotalBases + OnBaseEvents,
    GamePerformanceRate = ifelse(AB > 0, (TotalBases + OnBaseEvents) / AB, 0)
  ) %>%
  left_join(starting_pitchers, by = "game_pk") %>%
  filter(Team != pitcher_team)

# 4. Create Pitcher Elo Table Only for Involved Pitchers
Player_pitcher_elos <- Player_BatterWithPitcher %>%
  distinct(pitcher_pk) %>%
  rename(player_pk = pitcher_pk) %>%
  mutate(current_elo = 1000)

# === SLOPE TUNING FUNCTION === #
Player_tune_logistic_slope_logloss <- function(Player_BatterWithPitcher, Player_batter_elos_raw, Player_pitcher_elos_raw, slope_range = seq(1.0, 100.0, by = 1), K = 50, verbose = TRUE) {
  
  Player_center_value <- mean(Player_BatterWithPitcher$GamePerformanceRate, na.rm = TRUE)
  if (verbose) cat(sprintf("📊 Logistic center (mean GamePerformanceRate): %.3f\n", Player_center_value))
  
  log_loss <- function(actual, predicted, eps = 1e-15) {
    predicted <- pmin(pmax(predicted, eps), 1 - eps)
    -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
  }
  
  compute_logloss_for_slope <- function(slope) {
    Player_batter_elos <- Player_batter_elos_raw %>% rename(batter_pk = player_pk) %>% mutate(current_elo = 1000)
    Player_pitcher_elos <- Player_pitcher_elos_raw %>% rename(pitcher_pk = player_pk) %>% mutate(current_elo = 1000)
    loss_values <- c()
    
    for (i in 1:nrow(Player_BatterWithPitcher)) {
      row <- Player_BatterWithPitcher[i, ]
      b_id <- row$batter_pk
      p_id <- row$pitcher_pk
      g_perf <- row$GamePerformanceRate
      
      if (is.na(b_id) || is.na(p_id) || is.na(g_perf)) next
      
      b_index <- match(b_id, Player_batter_elos$batter_pk)
      p_index <- match(p_id, Player_pitcher_elos$pitcher_pk)
      if (is.na(b_index) || is.na(p_index)) next
      
      r_b <- Player_batter_elos$current_elo[b_index]
      r_p <- Player_pitcher_elos$current_elo[p_index]
      
      expected_batter <- 1 / (1 + 10^((r_p - r_b) / 400))
      actual_outcome <- 1 / (1 + exp(-slope * (g_perf - Player_center_value)))
      
      delta <- K * (actual_outcome - expected_batter)
      Player_batter_elos$current_elo[b_index] <- r_b + delta
      Player_pitcher_elos$current_elo[p_index] <- r_p - delta
      
      loss_values <- c(loss_values, log_loss(actual_outcome, expected_batter))
    }
    
    return(mean(loss_values, na.rm = TRUE))
  }
  
  logloss_results <- sapply(slope_range, compute_logloss_for_slope)
  best_slope <- slope_range[which.min(logloss_results)]
  
  # Plot result
  plot(slope_range, logloss_results, type = "b", pch = 19,
       col = "darkgreen", main = "Logistic Slope Tuning via Log Loss",
       xlab = "Logistic Slope", ylab = "Log Loss")
  abline(v = best_slope, col = "red", lty = 2)
  
  cat(sprintf("✅ Optimal slope (min log loss): %.3f\n", best_slope))
  
  return(list(
    best_slope = best_slope,
    slope_range = slope_range,
    logloss_values = logloss_results,
    Player_center_value = Player_center_value
  ))
}

# === Run slope tuning ===
Player_results_logloss <- Player_tune_logistic_slope_logloss(
  Player_BatterWithPitcher = Player_BatterWithPitcher,
  Player_batter_elos_raw = Player_batter_elos,
  Player_pitcher_elos_raw = Player_pitcher_elos
)

Player_optimal_slope <- Player_results_logloss$best_slope
Player_center_value <- Player_results_logloss$Player_center_value

# === Apply logistic score ===
Player_logistic_score <- function(x, center = Player_center_value, slope = Player_optimal_slope) {
  1 / (1 + exp(-slope * (x - center)))
}

Player_BatterWithPitcher <- Player_BatterWithPitcher %>%
  mutate(performance_score = Player_logistic_score(GamePerformanceRate))

# === ELO SYSTEM (UPDATED) === #
K <- 50
Player_elo_log <- list()

# Reinitialize Elo ratings for involved batters and pitchers
Player_elo_batters <- Player_BatterWithPitcher %>%
  distinct(batter_pk) %>%
  mutate(current_elo = 1000)

Player_elo_pitchers <- Player_BatterWithPitcher %>%
  distinct(pitcher_pk) %>%
  mutate(current_elo = 1000)

total <- nrow(Player_BatterWithPitcher)

for (i in 1:total) {
  row <- Player_BatterWithPitcher[i, ]
  b_id <- row$batter_pk
  p_id <- row$pitcher_pk
  
  b_index <- match(b_id, Player_elo_batters$batter_pk)
  p_index <- match(p_id, Player_elo_pitchers$pitcher_pk)
  
  if (is.na(b_index) || is.na(p_index) || is.na(row$performance_score)) next
  
  b_rating <- Player_elo_batters$current_elo[b_index]
  p_rating <- Player_elo_pitchers$current_elo[p_index]
  outcome <- row$performance_score
  
  e_batter <- 1 / (1 + 10^((p_rating - b_rating) / 400))
  delta <- K * (outcome - e_batter)
  
  Player_elo_batters$current_elo[b_index] <- b_rating + delta
  Player_elo_pitchers$current_elo[p_index] <- p_rating - delta
  
  Player_elo_log[[i]] <- data.frame(
    game_pk = row$game_pk,
    date = row$Date,
    batter_pk = b_id,
    pitcher_pk = p_id,
    batter_elo_before = b_rating,
    pitcher_elo_before = p_rating,
    batter_elo_after = b_rating + delta,
    pitcher_elo_after = p_rating - delta,
    outcome = outcome,
    expected_batter = e_batter,
    elo_delta = delta
  )
  
  if (i %% 100 == 0 || i == total) {
    cat(sprintf("🔄 Processed %d of %d games (%.1f%%)\n", i, total, 100 * i / total))
  }
}

# === FINAL OUTPUTS === #
PlayerEloLog2025 <- bind_rows(Player_elo_log)

PlayerFinalEloRatings2025 <- bind_rows(
  Player_elo_batters %>% rename(player_pk = batter_pk),
  Player_elo_pitchers %>% rename(player_pk = pitcher_pk)
)

PlayerFinalEloWithNames2025 <- PlayerFinalEloRatings2025 %>%
  left_join(Rosters2025 %>% select(player_pk, person_full_name, team_name, position_name), by = "player_pk") %>%
  distinct(player_pk, .keep_all = TRUE)

# Optional: Diagnostic checks
cat("📊 Pitchers updated:", length(unique(PlayerEloLog2025$pitcher_pk)), "\n")
cat("📊 Batters updated:", length(unique(PlayerEloLog2025$batter_pk)), "\n")


# ==================================================================================== #
#                                TEAM ELO SYSTEM — 2025
# ==================================================================================== #

# = Baseline Elo =
initial_elo <- 1000
K <- results_scaled$K

Team_Team_team_elos_2025 <- TeamList %>%
  select(team_id = id) %>%
  mutate(current_elo = initial_elo)

Team_expected_result <- function(elo_a, elo_b) {
  1 / (1 + 10 ^ ((elo_b - elo_a) / 400))
}

TeamEloLog2025 <- list()

for (i in 1:nrow(GameInformation2025)) {
  row <- GameInformation2025[i, ]
  if (is.na(row$teams.home.score) || is.na(row$teams.away.score)) next
  
  home_team <- row$teams.home.team.id
  away_team <- row$teams.away.team.id
  
  elo_home <- Team_Team_team_elos_2025$current_elo[Team_Team_team_elos_2025$team_id == home_team]
  elo_away <- Team_Team_team_elos_2025$current_elo[Team_Team_team_elos_2025$team_id == away_team]
  
  expected_home <- Team_expected_result(elo_home, elo_away)
  outcome_home <- ifelse(row$teams.home.score > row$teams.away.score, 1,
                         ifelse(row$teams.home.score < row$teams.away.score, 0, 0.5))
  
  # Elo Adjustment: Blowout = greater delta elo
  margin <- abs(row$teams.home.score - row$teams.away.score)
  margin_multiplier <- log(margin + 1)
  
  # Base Delta Elo 
  delta_home <- K * margin_multiplier * (outcome_home - expected_home)
  
  # Win streak dampening (3+ wins)
  if (i >= 5) {
    last_5 <- do.call(rbind, tail(TeamEloLog2025, 5))
    team_recent <- last_5 %>% filter(home_team_id == home_team)
    if (nrow(team_recent) >= 3 && sum(team_recent$outcome_home == 1) >= 3) {
      delta_home <- delta_home * 0.8
    }
  }
  
  # Loss streak penalty (3+ losses)
  if (i >= 5) {
    last_5 <- do.call(rbind, tail(TeamEloLog2025, 5))
    team_recent <- last_5 %>% filter(home_team_id == home_team)
    if (nrow(team_recent) >= 3 && sum(team_recent$outcome_home == 0) >= 3) {
      delta_home <- delta_home * 1.15
    }
  }
  
  # Penalize for losing badly to weak opponent
  if (outcome_home == 0 && elo_away < 1000 && margin >= 5) {
    delta_home <- delta_home * 1.25
  }
  
  # Season progress dynamic cap
  season_progress <- i / nrow(GameInformation2025)
  dynamic_cap <- max(results_scaled$cap * (1 - results_scaled$decay * season_progress), results_scaled$floor_val)
  
  delta_home <- max(min(delta_home, dynamic_cap), -dynamic_cap)
  delta_away <- -delta_home
  
  # Update Elo ratings
  Team_Team_team_elos_2025$current_elo[Team_Team_team_elos_2025$team_id == home_team] <- elo_home + delta_home
  Team_Team_team_elos_2025$current_elo[Team_Team_team_elos_2025$team_id == away_team] <- elo_away + delta_away
  
  # Regression toward mean every 15 games
  if (i %% 15 == 0) {
    Team_Team_team_elos_2025$current_elo <- Team_Team_team_elos_2025$current_elo * 0.985 + initial_elo * 0.015
  }
  
  TeamEloLog2025[[i]] <- data.frame(
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
}

TeamEloLog2025 <- bind_rows(TeamEloLog2025)

# Calculate average opponent Elo before each game
opponent_strength_2025 <- TeamEloLog2025 %>%
  transmute(team_id = home_team_id, opponent_elo = away_elo_before) %>%
  bind_rows(
    TeamEloLog2025 %>%
      transmute(team_id = away_team_id, opponent_elo = home_elo_before)
  ) %>%
  group_by(team_id) %>%
  summarise(avg_opp_elo = mean(opponent_elo), .groups = "drop")

# ==================================================================================== #
#                        ELO + WIN RATIO PLOT FOR INDIVIDUAL TEAM — 2025
# ==================================================================================== #

Team_plot_id <- 112   # e.g. Yankees
Team_plot_name <- TeamList %>% filter(id == Team_plot_id) %>% pull(name)

Team_team_elo_2025 <- TeamEloLog2025 %>%
  filter(home_team_id == Team_plot_id | away_team_id == Team_plot_id) %>%
  mutate(
    Team_team_elo = ifelse(home_team_id == Team_plot_id, home_elo_after, away_elo_after),
    opponent_id = ifelse(home_team_id == Team_plot_id, away_team_id, home_team_id),
    team_score = ifelse(home_team_id == Team_plot_id, home_score, away_score),
    opponent_score = ifelse(home_team_id == Team_plot_id, away_score, home_score),
    score_label = paste0(team_score, "–", opponent_score),
    outcome = case_when(
      team_score > opponent_score ~ "Win",
      team_score < opponent_score ~ "Loss",
      TRUE ~ "Tie"
    ),
    season = lubridate::year(as.Date(date)),
    win = ifelse(outcome == "Win", 1, 0),
    loss = ifelse(outcome == "Loss", 1, 0)
  ) %>%
  group_by(season) %>%
  mutate(
    cum_win = cumsum(win),
    cum_loss = cumsum(loss),
    win_ratio = cum_win / (cum_win + cum_loss),
    win_ratio_scaled = 500 + win_ratio * 1000
  ) %>%
  ungroup() %>%
  left_join(TeamList %>% select(id, opponent_abbr = abbreviation), by = c("opponent_id" = "id")) %>%
  arrange(as.Date(date)) %>%
  mutate(frame = row_number())

# === Final Point
final_point <- Team_team_elo_2025 %>%
  arrange(as.Date(date)) %>%
  slice_tail(n = 1)

# === Static Plot
ggplot(Team_team_elo_2025, aes(x = as.Date(date))) +
  geom_line(aes(y = Team_team_elo), color = "#1f1f1f", size = 1.1) +
  geom_line(aes(y = win_ratio_scaled), color = "#1b9e77", linetype = "dashed", size = 1) +
  geom_text(data = final_point,
            aes(y = Team_team_elo, label = paste0("Elo: ", round(Team_team_elo))),
            hjust = 1.1, vjust = -5.1, color = "#1f1f1f", fontface = "bold") +
  geom_text(data = final_point,
            aes(y = win_ratio_scaled, label = paste0("Win Ratio: ", scales::percent(win_ratio))),
            hjust = 1.1, vjust = -8.1, color = "#1b9e77", fontface = "bold") +
  scale_y_continuous(
    name = "Elo Rating",
    limits = c(500, 1500),
    sec.axis = sec_axis(~ (. - 500) / 1000, name = "Win Ratio", labels = percent_format())
  ) +
  scale_x_date(expand = expansion(mult = c(0.01, 0.05))) +
  labs(
    title = paste("📈 Elo & Win Ratio over Time for", Team_plot_name, "(2025)"),
    subtitle = "Solid = Elo | Dashed = Win Ratio",
    x = "Date", y = "Elo Rating"
  ) +
  theme_minimal(base_size = 14)

# === Animated Plot Over Time
Team_line_history <- do.call(rbind, lapply(1:nrow(Team_team_elo_2025), function(i) {
  Team_team_elo_2025[1:i, ] %>% mutate(frame = i)
}))

Team_label_history <- Team_line_history

Team_plot_2025 <- ggplot() +
  geom_line(data = Team_line_history, aes(x = as.Date(date), y = Team_team_elo, group = 1), color = "#1f1f1f", size = 1.1) +
  geom_point(data = Team_line_history, aes(x = as.Date(date), y = Team_team_elo), color = "#1f1f1f", size = 2) +
  geom_label_repel(
    data = Team_label_history,
    aes(x = as.Date(date), y = Team_team_elo, label = paste(opponent_abbr, score_label), fill = outcome, group = interaction(date, opponent_abbr)),
    size = 3.2, color = "white", fontface = "bold", show.legend = FALSE,
    max.overlaps = 50, seed = 42
  ) +
  scale_fill_manual(values = c("Win" = "#1b9e77", "Loss" = "#d95f02", "Tie" = "gray50")) +
  labs(
    title = paste("📈 Elo Trajectory for", Team_plot_name, "(2025)"),
    subtitle = "Line shows Elo change | Labels accumulate with score and result",
    x = "Date", y = "Elo Rating", fill = "Game Outcome"
  ) +
  theme_minimal(base_size = 14) +
  transition_manual(frame)

# === Render Animation
animate(Team_plot_2025, width = 900, height = 600, fps = 10, duration = 6, renderer = gifski_renderer())

# ==================================================================================== #
#                  FINAL TEAM SUMMARY TABLE FOR RANKING — 2025
# ==================================================================================== #

TeamFinalSummary2025 <- TeamEloLog2025 %>%
  transmute(team_id = home_team_id, result = outcome_home, elo_after = home_elo_after) %>%
  bind_rows(
    TeamEloLog2025 %>% transmute(team_id = away_team_id, result = 1 - outcome_home, elo_after = away_elo_after)
  ) %>%
  group_by(team_id) %>%
  summarise(
    games_played = n(),
    wins = sum(result == 1),
    losses = sum(result == 0),
    win_pct = round(wins / games_played, 3),
    final_elo = last(elo_after),
    .groups = "drop"
  ) %>%
  left_join(TeamList %>% select(team_id = id, team_name = name), by = "team_id") %>%
  left_join(opponent_strength_2025, by = "team_id") %>%
  select(team_id, team_name, games_played, wins, losses, win_pct, final_elo, avg_opp_elo) %>%
  arrange(desc(final_elo))

View(TeamFinalSummary2025)

# Final Elo vs Win Percentage (2025)
ggplot(TeamFinalSummary2025, aes(x = win_pct, y = final_elo)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  geom_text(aes(label = team_name), vjust = -1.1, size = 3) +
  labs(title = "Final Elo vs Win Percentage (2025)",
       x = "Win Percentage",
       y = "Final Elo") +
  theme_minimal(base_size = 14)

# Fit linear model
elo_model_2025 <- lm(final_elo ~ win_pct, data = TeamFinalSummary2025)

# Calculate RMSE
rmse_2025 <- sqrt(mean((TeamFinalSummary2025$final_elo - predict(elo_model_2025))^2))
print(paste("RMSE:", round(rmse_2025, 2)))

# Get R-squared
r_squared_2025 <- summary(elo_model_2025)$r.squared
print(paste("R-squared:", round(r_squared_2025, 4)))

# ==================================================================================== #
#                  PREDICTING RUNS SCORED (Single Season — 2025 Only)
# ==================================================================================== #

# Team name abbreviation mapping
fielding_map <- c(
  "Diamondbacks" = "ARI", "Braves" = "ATL", "Orioles" = "BAL", "Red Sox" = "BOS", "Cubs" = "CHC",
  "White Sox" = "CHW", "Reds" = "CIN", "Guardians" = "CLE", "Rockies" = "COL", "Tigers" = "DET",
  "Astros" = "HOU", "Royals" = "KCR", "Angels" = "LAA", "Dodgers" = "LAD", "Marlins" = "MIA",
  "Brewers" = "MIL", "Twins" = "MIN", "Mets" = "NYM", "Yankees" = "NYY", "Athletics" = "ATH",
  "Phillies" = "PHI", "Pirates" = "PIT", "Padres" = "SDP", "Giants" = "SFG", "Mariners" = "SEA",
  "Cardinals" = "STL", "Rays" = "TBR", "Rangers" = "TEX", "Blue Jays" = "TOR", "Nationals" = "WSN"
)

# Helper to merge stats
merge_stats <- function(data, stats, team_col, prefix) {
  stats_renamed <- stats %>%
    rename_with(~ paste0(prefix, "_", .x), -team_name)
  data %>%
    left_join(stats_renamed, by = setNames("team_name", team_col))
}

# === Build GameData for 2025 === #

GameData2025 <- GameInformation2025 %>%
  rename(
    home_runs = teams.home.score,
    away_runs = teams.away.score
  )

# Get Elo values
GameData2025 <- GameData2025 %>%
  left_join(TeamEloLog2025 %>% select(game_pk, home_elo_before, away_elo_before), by = "game_pk")

# Team stats for 2025
batting_stats_2025 <- TeamBattingStatistics2025 %>%
  mutate(
    wRC = wRC / G,
    R = R / G
  ) %>%
  select(team_name, OBP, SLG, AVG, wRC, R)

pitching_stats_2025 <- TeamPitchingStatistics2025 %>%
  select(team_name, ERA, WHIP, K_9, BB_9, HR_9)

fielding_stats_2025 <- TeamFieldingStatistics2025 %>%
  mutate(
    team_name = fielding_map[team_name],
    DRS = DRS / G,
    Defense = Defense / G
  ) %>%
  filter(!is.na(team_name)) %>%
  select(team_name, DRS, Defense)
# NO UZR (2021 - 2024 datasets included UZR)

# Merge all stats by team side
GameData2025 <- GameData2025 %>%
  merge_stats(batting_stats_2025, "home_team_abbr", "home") %>%
  merge_stats(pitching_stats_2025, "home_team_abbr", "home") %>%
  merge_stats(fielding_stats_2025, "home_team_abbr", "home") %>%
  merge_stats(batting_stats_2025, "away_team_abbr", "away") %>%
  merge_stats(pitching_stats_2025, "away_team_abbr", "away") %>%
  merge_stats(fielding_stats_2025, "away_team_abbr", "away")

# Final dataset for modeling
TrainingData2025 <- GameData2025 %>%
  select(
    game_pk, officialDate, season, home_team_abbr, away_team_abbr,
    home_elo_before, away_elo_before, home_runs, away_runs,
    starts_with("home_"), starts_with("away_")
  )

# Remove rows with missing values
TrainingDataClean2025 <- TrainingData2025 %>% na.omit()

# ===================================== #
#   PREDICTING RUNS ON 2025 DATA
# ===================================== #

# Apply chosen model to 2025 dataset
TrainingDataClean2025 <- TrainingDataClean2025 %>%
  mutate(
    predicted_home_runs = case_when(
      best_home_model == "poisson_home" ~ predict(glm_home, newdata = TrainingDataClean2025, type = "response"),
      best_home_model == "linear_home" ~ predict(lm_home, newdata = TrainingDataClean2025),
      best_home_model == "gbm_home" ~ predict(gbm_home, newdata = TrainingDataClean2025, n.trees = best_iter_home)
    ),
    predicted_away_runs = case_when(
      best_away_model == "poisson_away" ~ predict(glm_away, newdata = TrainingDataClean2025, type = "response"),
      best_away_model == "linear_away" ~ predict(lm_away, newdata = TrainingDataClean2025),
      best_away_model == "gbm_away" ~ predict(gbm_away, newdata = TrainingDataClean2025, n.trees = best_iter_away)
    )
  )

# === ATTACH TO TeamEloLog USING game_pk === #
predictions2025 <- TrainingDataClean2025 %>%
  select(game_pk, predicted_home_runs, predicted_away_runs)

TeamEloLog2025 <- TeamEloLog2025 %>%
  left_join(predictions2025, by = "game_pk")

# ================================================ #
# PREDICTING WINNER BASED ON PREDICTED RUNS SCORED — 2025
# ================================================ #

# Ensure required data is present and valid
TrainingDataClean2025 <- TrainingDataClean2025 %>%
  mutate(
    actual_winner = ifelse(home_runs > away_runs, 1, 0),
    predicted_diff = predicted_home_runs - predicted_away_runs
  )

# Remove any rows with missing values in key columns before modeling
TrainingDataValid2025 <- TrainingDataClean2025 %>%
  filter(!is.na(actual_winner), !is.na(predicted_home_runs), !is.na(predicted_away_runs))

# 1️⃣ Fit logistic regression model
logit_win_model_2025 <- glm(actual_winner ~ predicted_diff, data = TrainingDataValid2025, family = "binomial")
TrainingDataValid2025$win_prob_logistic <- predict(logit_win_model_2025, newdata = TrainingDataValid2025, type = "response")
TrainingDataValid2025$pred_logistic_winner <- ifelse(TrainingDataValid2025$win_prob_logistic > 0.5, 1, 0)
accuracy_logistic_2025 <- mean(TrainingDataValid2025$pred_logistic_winner == TrainingDataValid2025$actual_winner)

# 2️⃣ Tune Pythagorean exponent for 2025
exponents_2025 <- c(1.83, 2)
pythag_accuracy_2025 <- numeric(length(exponents_2025))

for (i in seq_along(exponents_2025)) {
  exp_val <- exponents_2025[i]
  
  # Compute only on rows where both predicted runs are positive
  valid <- TrainingDataValid2025 %>%
    filter(predicted_home_runs > 0, predicted_away_runs > 0)
  
  probs <- (valid$predicted_home_runs^exp_val) / 
    ((valid$predicted_home_runs^exp_val) + (valid$predicted_away_runs^exp_val))
  
  preds <- ifelse(probs > 0.5, 1, 0)
  pythag_accuracy_2025[i] <- mean(preds == valid$actual_winner)
}

# Find best exponent
best_exp_2025 <- exponents_2025[which.max(pythag_accuracy_2025)]
best_acc_2025 <- max(pythag_accuracy_2025)

cat("🔍 Best Pythagorean Exponent (2025):", best_exp_2025, "\n")
cat("⚾ Best Pythagorean Accuracy (2025):", round(best_acc_2025 * 100, 2), "%\n")
cat("📈 Logistic Accuracy (2025):", round(accuracy_logistic_2025 * 100, 2), "%\n")

# 3️⃣ Apply best method to TeamEloLog2025
# Apply Best Model to TeamEloLog2025
TeamEloLog2025 <- TeamEloLog2025 %>%
  select(-predicted_home_runs, -predicted_away_runs) %>%  # remove existing if they exist
  left_join(
    TrainingDataClean2025 %>% select(game_pk, predicted_home_runs, predicted_away_runs), 
    by = "game_pk"
  ) %>%
  mutate(
    predicted_diff = predicted_home_runs - predicted_away_runs
  )


if (accuracy_logistic_2025 >= best_acc_2025) {
  cat("✅ Using Logistic Regression for win probability\n")
  TeamEloLog2025 <- TeamEloLog2025 %>%
    mutate(
      home_win_probability = predict(logit_win_model_2025, newdata = ., type = "response"),
      away_win_probability = 1 - home_win_probability
    )
} else {
  cat("✅ Using Pythagorean Expectation with exponent =", best_exp_2025, "\n")
  TeamEloLog2025 <- TeamEloLog2025 %>%
    mutate(
      home_win_probability = (predicted_home_runs^best_exp_2025) /
        ((predicted_home_runs^best_exp_2025) + (predicted_away_runs^best_exp_2025)),
      away_win_probability = 1 - home_win_probability
    )
}

TeamEloLog2025 <- TeamEloLog2025 %>%
  left_join(TeamList %>% select(home_team_id = id, home_team_name = name), by = "home_team_id") %>%
  left_join(TeamList %>% select(away_team_id = id, away_team_name = name), by = "away_team_id")

# ==================================================================================== #
#                           PART 3: Exporting Data & Plots
# ==================================================================================== #

# === Define export path ===
export_path <- "C:/Users/micha/OneDrive/Desktop/BaseballProject/data"

# === Helper function ===
safe_export <- function(df_name) {
  tryCatch({
    df <- get(df_name)
    write_csv(df, file.path(export_path, paste0(df_name, ".csv")))
    cat("✅ Exported:", df_name, "\n")
  }, error = function(e) {
    cat("❌ Could not export:", df_name, "—", conditionMessage(e), "\n")
  })
}

# === Key data frames to export ===
export_list <- c(
  # Final summaries
  "TeamFinalSummary",
  "TeamFinalSummary2025",
  "PlayerFinalEloWithNames2025",
  
  # Game-level predictions
  "TrainingDataClean",
  "TrainingDataClean2025",
  "TeamEloLog",
  "TeamEloLog2025",
  "PlayerEloLog2025",
  
  # Team stats (2021–2025)
  paste0("TeamBattingStatistics", 2021:2025),
  paste0("TeamPitchingStatistics", 2021:2025),
  paste0("TeamFieldingStatistics", 2021:2025),
  
  # Player stats
  "PlayerBattingStatistics2025",
  "PlayerFieldingStatistics2025",
  "BatterGameLogs2025",
  "PitcherGameLogs2025",
  
  # Game metadata
  "MasterGameInformation",
  "GameInformation2025"
)

# === Export all ===
lapply(export_list, safe_export)

# PLOTS
# === Output path for images ===
plot_path <- "C:/Users/micha/OneDrive/Desktop/BaseballProject/static/images"

# === Helper function to export ggplot ===
export_plot <- function(plot_obj, filename, width = 10, height = 6, dpi = 300) {
  ggsave(filename = file.path(plot_path, paste0(filename, ".png")),
         plot = plot_obj, width = width, height = height, dpi = dpi)
  cat("✅ Saved:", filename, "\n")
}

# === Plots from your script ===

# 1️⃣ Grid Search Performance Plot
export_plot(TeamEloParameterPlot, "TeamElo_GridSearch_Performance")

# 2️⃣ Final Elo vs Win Percentage (2021–2024)
plot_elo_legacy <- ggplot(TeamFinalSummary, aes(x = win_pct, y = final_elo)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  geom_text(aes(label = team_name), vjust = -1.1, size = 3) +
  labs(title = "Final Elo vs Win Percentage (2021–2024)",
       x = "Win Percentage", y = "Final Elo") +
  theme_minimal(base_size = 14)
export_plot(plot_elo_legacy, "FinalElo_vs_WinPct_2021_2024")

# 3️⃣ Final Elo vs Win Percentage (2025)
plot_elo_2025 <- ggplot(TeamFinalSummary2025, aes(x = win_pct, y = final_elo)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  geom_text(aes(label = team_name), vjust = -1.1, size = 3) +
  labs(title = "Final Elo vs Win Percentage (2025)",
       x = "Win Percentage", y = "Final Elo") +
  theme_minimal(base_size = 14)
export_plot(plot_elo_2025, "FinalElo_vs_WinPct_2025")

# 4️⃣ Elo & Win Ratio Over Time (2025 - Team ID 112)
export_plot(
  ggplot(Team_team_elo_2025, aes(x = as.Date(date))) +
    geom_line(aes(y = Team_team_elo), color = "#1f1f1f", size = 1.1) +
    geom_line(aes(y = win_ratio_scaled), color = "#1b9e77", linetype = "dashed", size = 1) +
    geom_text(data = final_point,
              aes(y = Team_team_elo, label = paste0("Elo: ", round(Team_team_elo))),
              hjust = 1.1, vjust = -5.1, color = "#1f1f1f", fontface = "bold") +
    geom_text(data = final_point,
              aes(y = win_ratio_scaled, label = paste0("Win Ratio: ", scales::percent(win_ratio))),
              hjust = 1.1, vjust = -8.1, color = "#1b9e77", fontface = "bold") +
    scale_y_continuous(
      name = "Elo Rating", limits = c(500, 1500),
      sec.axis = sec_axis(~ (. - 500) / 1000, name = "Win Ratio", labels = scales::percent_format())
    ) +
    scale_x_date(expand = expansion(mult = c(0.01, 0.05))) +
    labs(
      title = paste("📈 Elo & Win Ratio over Time for", Team_plot_name, "(2025)"),
      subtitle = "Solid = Elo | Dashed = Win Ratio",
      x = "Date", y = "Elo Rating"
    ) +
    theme_minimal(base_size = 14),
  "Elo_WinRatio_OverTime_2025_Team112")