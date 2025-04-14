# ==================================================================================== #
#                                     DATA PREPARATION 
# ==================================================================================== #
rm(list = ls())

library(baseballr)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(gganimate)

# ================ #
# Team Information
# ================ #
teams <- teams_lu_table %>%
  filter(sport.name == "Major League Baseball")

TeamList = teams %>%
  select(id, name, bref_abbreviation, abbreviation, venue.id, venue.name, league.id, league.name, division.id, division.name)

# ====================== #
# IMPORTANT: SELECT YEAR
# ====================== #
target_year = 2025

# ================= #
# 2024 Team Rosters
# ================= #

teams_roster <- data.frame()

for (i in 1:nrow(TeamList)) {
  team_id <- TeamList$id[i]  # Use "id" column for team_id
  team_name <- TeamList$name[i]
  team_abbreviation <- TeamList$abbreviation[i]
  
  cat("Fetching roster for:", team_name, "(", team_abbreviation, ")\n")
  
  tryCatch({
    roster <- mlb_rosters(team_id = team_id, season = target_year, roster_type = "active")
    
    roster$team_id <- team_id
    roster$team_name <- team_name
    roster$team_abbreviation <- team_abbreviation
    
    teams_roster <- bind_rows(teams_roster, roster)
    
    cat("✅ Successfully added roster for", team_name, "\n")
  }, error = function(e) {
    cat("❌ Error retrieving roster for", team_name, ":", conditionMessage(e), "\n")
  })
}

Rosters = teams_roster %>%
  filter(teams_roster$status_description == "Active") %>%
  distinct(person_id, .keep_all = TRUE) %>%
  select(
    season, 
    team_id, team_abbreviation, team_name, 
    person_id, person_full_name, position_name, position_abbreviation, status_description
  )

# ===================== #
# Creating a Unified PK #
# ===================== #
chadwick_raw <- chadwick_player_lu() 
print("Chadwick lookup columns:") 
print(colnames(chadwick_raw))

chadwick_lookup <- chadwick_raw %>% filter(!is.na(key_fangraphs)) %>% select(mlb_id = key_mlbam, fangraphs_id = key_fangraphs) 
print("Rosters columns before joining:") 
print(colnames(Rosters))
Rosters <- Rosters %>% left_join(chadwick_lookup, by = c("person_id" = "mlb_id")) %>% mutate(player_pk = ifelse(!is.na(fangraphs_id), fangraphs_id, person_id) ) %>% select(season, team_id, team_abbreviation, team_name, person_id, person_full_name, position_name, position_abbreviation, status_description, player_pk)


# ======================= #
# Team Statistics in 2024
# ======================= #
TeamFieldingStatistics = fg_team_fielder(startseason = target_year, endseason = target_year, qual = 'y')
TeamBattingStatistics = fg_team_batter(startseason = target_year, endseason = target_year, qual = 'y')
TeamPitchingStatistics = fg_team_pitcher(startseason = target_year, endseason = target_year, qual = 'y')


# ========================= #
# Player Statistics in 2024
# ========================= #
PlayerFieldingStatistics = fg_fielder_leaders(startseason = target_year, endseason = target_year)
PlayerBattingStatistics = fg_batter_leaders(startseason = target_year, endseason = target_year)

# ======================== #
# Game Information in 2024
# ======================== #

# Date Range
dates <- seq.Date(as.Date(paste0(as.character(target_year), "-01-01")), as.Date(paste0(as.character(target_year), "-12-31")), by = "day")
dates <- as.character(dates)  # Explicitly convert to character strings

game_info_list <- list()

instance_count <- 0  
total_iterations <- length(dates)  

for (date in dates) {
  instance_count <- instance_count + 1  
  cat("[", instance_count, "/", total_iterations, "] Fetching games for date:", date, "\n")
  
  tryCatch({
    df <- mlb_game_pks(date = date, level_ids = c(1))  
    
    if (is.data.frame(df) && nrow(df) > 0) {
      df <- df %>% mutate(game_date = date) 
      game_info_list[[date]] <- df
      cat("✅ Data retrieved for", date, "- Rows:", nrow(df), "\n")
    } else {
      cat("⚠️ No games found for", date, "\n")
    }
  }, error = function(e) {
    cat("❌ Error fetching data for", date, ":", conditionMessage(e), "\n")
  })
}


game_info <- bind_rows(game_info_list)

GameInformation <- game_info %>%
  select(
    game_pk,
    season,
    officialDate,
    gameNumber,
    dayNight,
    gamesInSeries,
    seriesGameNumber,
    seriesDescription,
    
    teams.away.score,
    teams.away.isWinner,
    teams.away.seriesNumber,
    teams.away.leagueRecord.wins,
    teams.away.leagueRecord.losses,
    teams.away.leagueRecord.pct,
    teams.away.team.id,
    teams.away.team.name,
    
    teams.home.score,
    teams.home.isWinner,
    teams.home.seriesNumber,
    teams.home.leagueRecord.wins,
    teams.home.leagueRecord.losses,
    teams.home.leagueRecord.pct,
    teams.home.team.id,
    teams.home.team.name
  )


# Hard coding abbreviations to match GameInformation with dataset teams

team_abbr_lookup <- tibble::tibble(
  team_name = c(
    "Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles", "Boston Red Sox", "Chicago Cubs",
    "Chicago White Sox", "Cincinnati Reds", "Cleveland Guardians", "Colorado Rockies", "Detroit Tigers",
    "Houston Astros", "Kansas City Royals", "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins",
    "Milwaukee Brewers", "Minnesota Twins", "New York Mets", "New York Yankees", "Athletics",
    "Philadelphia Phillies", "Pittsburgh Pirates", "San Diego Padres", "San Francisco Giants",
    "Seattle Mariners", "St. Louis Cardinals", "Tampa Bay Rays", "Texas Rangers", "Toronto Blue Jays",
    "Washington Nationals"
  ),
  abbreviation = c(
    "ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", 
    "HOU", "KCR", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "ATH", 
    "PHI", "PIT", "SDP", "SFG", "SEA", "STL", "TBR", "TEX", "TOR", "WSN"
  )
)

GameInformation <- GameInformation %>%
  filter(seriesDescription == "Regular Season") %>%
  left_join(team_abbr_lookup, by = c("teams.home.team.name" = "team_name")) %>%
  rename(home_team_abbr = abbreviation) %>%
  left_join(team_abbr_lookup, by = c("teams.away.team.name" = "team_name")) %>%
  rename(away_team_abbr = abbreviation) %>%
  mutate(Date_Home_Away = paste(officialDate, home_team_abbr, away_team_abbr, sep = "_")) %>%
  filter(!is.na(teams.away.score)) %>%
  distinct(game_pk, .keep_all = TRUE) %>%
  distinct(Date_Home_Away, .keep_all = TRUE)



cat("\n✅ Scraping complete. Collected game data for all of 2024.\n")


# ============================= #
# Batting & Pitching Statistics 
# ============================= #

baseball_data <- list()

all_players <- chadwick_player_lu() %>%
  filter(!is.na(key_fangraphs), mlb_played_last == target_year) %>%  # Filter players who played in 2025
  select(player_id = key_fangraphs)

years <- target_year:target_year 

scrape_batter_game_logs <- function() {
  batter_logs <- list()
  instance_count <- 0  # Initialize counter
  
  total_iterations <- length(years) * nrow(all_players)  # Total expected iterations
  
  for (year in years) {
    cat("\n=== Scraping batter logs for year:", year, "===\n")
    
    for (player in all_players$player_id) {
      instance_count <- instance_count + 1 
      cat("[", instance_count, "/", total_iterations, "] Processing PlayerID:", player, "Year:", year, "\n")
      
      tryCatch({
        df <- fg_batter_game_logs(playerid = player, year = year)
        
        if (is.data.frame(df) && nrow(df) > 0) {
          df <- df %>% mutate(player_id = player, year = year)
          batter_logs[[paste0(player, "_", year)]] <- df
          cat("✅ Added data for PlayerID:", player, "Year:", year, "- Rows:", nrow(df), "\n")
        } else {
          cat("⚠️ No data for PlayerID:", player, "Year:", year, "\n")
        }
      }, error = function(e) {
        cat("❌ Error for PlayerID:", player, "Year:", year, ":", conditionMessage(e), "\n")
      })
    }
  }
  
  batter_game_logs_df <- bind_rows(batter_logs)
  return(batter_game_logs_df)
}

scrape_pitcher_game_logs <- function() {
  pitcher_logs <- list()
  instance_count <- 0  
  
  total_iterations <- length(years) * nrow(all_players)  
  
  for (year in years) {
    cat("\n=== Scraping pitcher logs for year:", year, "===\n")
    
    for (player in all_players$player_id) {
      instance_count <- instance_count + 1  
      cat("[", instance_count, "/", total_iterations, "] Processing PlayerID:", player, "Year:", year, "\n")
      
      tryCatch({
        df <- fg_pitcher_game_logs(playerid = player, year = year)
        
        if (is.data.frame(df) && nrow(df) > 0) {
          df <- df %>% mutate(player_id = player, year = year)  # Add player ID and year columns
          pitcher_logs[[paste0(player, "_", year)]] <- df
          cat("✅ Added data for PlayerID:", player, "Year:", year, "- Rows:", nrow(df), "\n")
        } else {
          cat("⚠️ No data for PlayerID:", player, "Year:", year, "\n")
        }
      }, error = function(e) {
        cat("❌ Error for PlayerID:", player, "Year:", year, ":", conditionMessage(e), "\n")
      })
    }
  }
  
  pitcher_game_logs_df <- bind_rows(pitcher_logs)
  return(pitcher_game_logs_df)
}

baseball_data$batter_game_logs <- scrape_batter_game_logs()
baseball_data$pitcher_game_logs <- scrape_pitcher_game_logs()

# BATTER GAME LOGS #
BatterGameLogs = baseball_data$batter_game_logs %>%
  filter(AB > 0)

BatterGameLogs <- BatterGameLogs %>%
  mutate(
    Home_Away = ifelse(startsWith(Opp, "@"), "Away", "Home"),
    HomeTeam = ifelse(startsWith(Opp, "@"), substr(Opp, 2, 4), Team),
    AwayTeam = ifelse(startsWith(Opp, "@"), Team, Opp),
    Date_Home_Away = paste(Date, HomeTeam, AwayTeam, sep = "_")
  ) %>%
  left_join(
    GameInformation %>% select(Date_Home_Away, game_pk),
    by = "Date_Home_Away"
  )



# PITCHER GAME LOGS #
PitcherGameLogs = baseball_data$pitcher_game_logs

PitcherGameLogs <- PitcherGameLogs %>%
  mutate(
    HomeTeam = ifelse(startsWith(Opp, "@"), substr(Opp, 2, 4), Team),
    AwayTeam = ifelse(startsWith(Opp, "@"), Team, Opp),
    Date_Home_Away = paste(Date, HomeTeam, AwayTeam, sep = "_")
  ) %>%
  left_join(
    GameInformation %>% select(Date_Home_Away, game_pk),
    by = "Date_Home_Away"
  )


# ========================= #
# Incorporating Unified PKs 
# ========================= #

BatterGameLogs <- BatterGameLogs %>% rename(player_pk = playerid) 
PitcherGameLogs <- PitcherGameLogs %>% rename(player_pk = playerid) 
PlayerBattingStatistics <- PlayerBattingStatistics %>% rename(player_pk = playerid) 
PlayerFieldingStatistics <- PlayerFieldingStatistics %>% rename(player_pk = playerid)

print("Unified primary key created successfully in Rosters and propagated to player-level datasets.")

# ================== #
# Exporting as a CSV
# ================== #

export_dir <- "C:/Users/micha/OneDrive/Desktop/MLBData"


teams_path <- file.path(export_dir, "TeamList.csv")
write_csv(TeamList, teams_path)
cat("📂 Team Information: ", teams_path, "\n")
teams_roster_path <- file.path(export_dir, "TeamRosters.csv")
write_csv(Rosters, teams_roster_path)
cat("📂 Team Rosters: ", teams_roster_path, "\n")


TeamBattingStatistics_path <- file.path(export_dir, "TeamBattingStatistics.csv")
write.csv(TeamBattingStatistics, TeamBattingStatistics_path)
cat("📂 Team - Batting Statistics: ", TeamBattingStatistics_path, "\n")
TeamPitchingStatistics_path <- file.path(export_dir, "TeamPitchingStatistics.csv")
write.csv(TeamPitchingStatistics, TeamPitchingStatistics_path)
cat("📂 Team - Pitching Statistics: ", TeamPitchingStatistics_path, "\n")
TeamFieldingStatistics_path <- file.path(export_dir, "TeamFieldingStatistics.csv")
write.csv(TeamFieldingStatistics, TeamFieldingStatistics_path)
cat("📂 Team - Fielding Statistics: ", TeamFieldingStatistics_path, "\n")

PlayerBattingStatistics_path <- file.path(export_dir, "PlayerBattingStatistics.csv")
write_csv(PlayerBattingStatistics, PlayerBattingStatistics_path)
cat("📂 Player - Batting Statistics: ", PlayerBattingStatistics_path, "\n")
PlayerFieldingStatistics_path <- file.path(export_dir, "PlayerFieldingStatistics.csv")
write_csv(PlayerFieldingStatistics, PlayerFieldingStatistics_path)
cat("📂 Player - Fielding Statistics: ", PlayerFieldingStatistics_path, "\n")

game_info_path <- file.path(export_dir, "GameInformation.csv")
write_csv(GameInformation, game_info_path)
cat("📂 Game Information: ", game_info_path, "\n")
batter_csv_path <- file.path(export_dir, "BatterGameLogs.csv")
write_csv(BatterGameLogs, batter_csv_path)
cat("📂 Batter Logs: ", batter_csv_path, "\n")
pitcher_csv_path <- file.path(export_dir, "PitcherGameLogs.csv")
write_csv(PitcherGameLogs, pitcher_csv_path)
cat("📂 Pitcher Logs: ", pitcher_csv_path, "\n")


# ==================================================================================== #
#                               PLAYER ELO SYSTEM
# ==================================================================================== #

# === SETUP === #

# Initialize Elo Ratings
Player_batter_elos <- Rosters %>%
  distinct(player_pk) %>%
  mutate(current_elo = 1000)

Player_pitcher_elos <- Rosters %>%
  distinct(player_pk) %>%
  mutate(current_elo = 1000)

starting_pitchers <- PitcherGameLogs %>%
  filter(GS == 1) %>%
  select(game_pk, pitcher_team = Team, pitcher_pk = player_pk)

Player_BatterWithPitcher <- BatterGameLogs %>%
  select(game_pk, batter_pk = player_pk, Team, Date, `1B`, `2B`, `3B`, HR, BB, IBB, HBP, AB) %>%
  mutate(across(c(`1B`, `2B`, `3B`, HR, BB, IBB, HBP, AB), as.numeric)) %>%
  mutate(
    UBB = BB - IBB,
    TotalBases = `1B` + 2 * `2B` + 3 * `3B` + 4 * HR,
    OnBaseEvents = UBB + HBP,
    GamePerformance = TotalBases + OnBaseEvents
  ) %>%
  left_join(starting_pitchers, by = "game_pk") %>%
  filter(Team != pitcher_team)


Player_tune_logistic_slope_logloss <- function(Player_BatterWithPitcher, Player_batter_elos_raw, Player_pitcher_elos_raw, slope_range = seq(0.0, 10.0, by = 0.05), K = 12.6, verbose = TRUE) {

  Player_center_value <- mean(Player_BatterWithPitcher$GamePerformance, na.rm = TRUE)
  if (verbose) cat(sprintf("📊 Logistic center (mean GamePerformance): %.3f\n", Player_center_value))
  
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
      g_perf <- row$GamePerformance
      
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


Player_results_logloss <- Player_tune_logistic_slope_logloss(
  Player_BatterWithPitcher = Player_BatterWithPitcher,
  Player_batter_elos_raw = Player_batter_elos,
  Player_pitcher_elos_raw = Player_pitcher_elos
)

Player_optimal_slope = Player_results_logloss$best_slope



Player_center_value <- mean(Player_BatterWithPitcher$GamePerformance, na.rm = TRUE)

Player_logistic_score <- function(x, center = Player_center_value, slope = Player_optimal_slope) {
  1 / (1 + exp(-slope * (x - center)))
}

Player_BatterWithPitcher <- Player_BatterWithPitcher %>%
  mutate(performance_score = Player_logistic_score(GamePerformance))


# === ELO SYSTEM === #
K <- 12.6
Player_elo_log <- list()

Player_elo_batters <- Player_batter_elos %>% rename(batter_pk = player_pk)
Player_elo_pitchers <- Player_pitcher_elos %>% rename(pitcher_pk = player_pk)

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

PlayerEloLog <- bind_rows(Player_elo_log)

PlayerFinalEloRatings <- bind_rows(
  Player_elo_batters %>% rename(player_pk = batter_pk),
  Player_elo_pitchers %>% rename(player_pk = pitcher_pk)
)

PlayerFinalEloWithNames <- PlayerFinalEloRatings %>%
  left_join(Rosters %>% select(player_pk, person_full_name, team_name, position_name),
            by = "player_pk") %>%
  distinct(player_pk, .keep_all = TRUE)


# ==================================================================================== #
#                                   TEAM ELO SYSTEM       
# ==================================================================================== #

initial_elo <- 1500
Team_Team_team_elos <- TeamList %>%
  select(team_id = id) %>%
  mutate(current_elo = initial_elo)

Team_expected_result <- function(elo_a, elo_b) {
  1 / (1 + 10 ^ ((elo_b - elo_a) / 400))
}

K <- 20 
TeamEloLog <- list()

for (i in 1:nrow(GameInformation)) {
  row <- GameInformation[i, ]
  
  if (is.na(row$teams.home.score) || is.na(row$teams.away.score)) next
  
  home_team <- row$teams.home.team.id
  away_team <- row$teams.away.team.id
  
  elo_home <- Team_Team_team_elos$current_elo[Team_Team_team_elos$team_id == home_team]
  elo_away <- Team_Team_team_elos$current_elo[Team_Team_team_elos$team_id == away_team]
  
  expected_home <- Team_expected_result(elo_home, elo_away)
  outcome_home <- ifelse(row$teams.home.score > row$teams.away.score, 1,
                         ifelse(row$teams.home.score < row$teams.away.score, 0, 0.5))
  
  delta_home <- K * (outcome_home - expected_home)
  delta_away <- -delta_home
  
  Team_Team_team_elos$current_elo[Team_Team_team_elos$team_id == home_team] <- elo_home + delta_home
  Team_Team_team_elos$current_elo[Team_Team_team_elos$team_id == away_team] <- elo_away + delta_away
  
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


# === Choose Team ID ===
Team_plot_id <- 112   # Chicago Cubs

Team_plot_name <- TeamList %>% filter(id == Team_plot_id) %>% pull(name)

# === Prepare Base Data ===
Team_team_elo <- TeamEloLog %>%
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
    )
  ) %>%
  left_join(TeamList %>% select(id, opponent_abbr = abbreviation), by = c("opponent_id" = "id")) %>%
  arrange(as.Date(date)) %>%
  mutate(frame = row_number())  # For animation

# === Build Line & Label Histories ===

Team_line_history <- do.call(rbind, lapply(1:nrow(Team_team_elo), function(i) {
  Team_team_elo[1:i, ] %>% mutate(frame = i)
}))

Team_label_history <- Team_line_history  # same structure

# === Create Animated Plot ===
Team_plot <- ggplot() +
  geom_line(data = Team_line_history, aes(x = as.Date(date), y = Team_team_elo, group = 1), color = "#1f1f1f", size = 1.1) +
  
  geom_point(data = Team_line_history, aes(x = as.Date(date), y = Team_team_elo), color = "#1f1f1f", size = 2) +
  
  geom_label_repel(
    data = Team_label_history,
    aes(x = as.Date(date), y = Team_team_elo, label = paste(opponent_abbr, score_label), fill = outcome, group = interaction(date, opponent_abbr)),
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
    x = "Date",
    y = "Elo Rating",
    fill = "Game Outcome"
  ) +
  theme_minimal(base_size = 14) +
  
  transition_manual(frame)

# === Render Animation ===
animate(Team_plot, width = 900, height = 600, fps = 10, duration = 6, renderer = gifski_renderer())
