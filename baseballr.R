rm(list = ls())

library(baseballr)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

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


# === #
# GAME-LEVEL ELO SYSTEM
# === #
library(dplyr)

# === SETUP === #

# 1. Initialize Elo Ratings
batter_elos <- Rosters %>%
  distinct(player_pk) %>%
  mutate(current_elo = 1000)

pitcher_elos <- Rosters %>%
  distinct(player_pk) %>%
  mutate(current_elo = 1000)

# 2. Identify Starting Pitchers
starting_pitchers <- PitcherGameLogs %>%
  filter(GS == 1) %>%
  select(game_pk, pitcher_team = Team, pitcher_pk = player_pk)

# 3. Prepare Batter Logs with Performance Metric
BatterWithPitcher <- BatterGameLogs %>%
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

# 4. Logistic Scoring Function (performance score ∈ [0, 1])

tune_logistic_slope_logloss <- function(BatterWithPitcher, batter_elos_raw, pitcher_elos_raw, slope_range = seq(0.0, 10.0, by = 0.05), K = 12.6, verbose = TRUE) {

  # 📊 Compute center of GamePerformance
  center_value <- mean(BatterWithPitcher$GamePerformance, na.rm = TRUE)
  if (verbose) cat(sprintf("📊 Logistic center (mean GamePerformance): %.3f\n", center_value))
  
  # 🔧 Log loss calculator
  log_loss <- function(actual, predicted, eps = 1e-15) {
    predicted <- pmin(pmax(predicted, eps), 1 - eps)
    -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
  }
  
  # 🎯 Score one slope by log loss
  compute_logloss_for_slope <- function(slope) {
    batter_elos <- batter_elos_raw %>% rename(batter_pk = player_pk) %>% mutate(current_elo = 1000)
    pitcher_elos <- pitcher_elos_raw %>% rename(pitcher_pk = player_pk) %>% mutate(current_elo = 1000)
    loss_values <- c()
    
    for (i in 1:nrow(BatterWithPitcher)) {
      row <- BatterWithPitcher[i, ]
      b_id <- row$batter_pk
      p_id <- row$pitcher_pk
      g_perf <- row$GamePerformance
      
      if (is.na(b_id) || is.na(p_id) || is.na(g_perf)) next
      
      b_index <- match(b_id, batter_elos$batter_pk)
      p_index <- match(p_id, pitcher_elos$pitcher_pk)
      if (is.na(b_index) || is.na(p_index)) next
      
      r_b <- batter_elos$current_elo[b_index]
      r_p <- pitcher_elos$current_elo[p_index]
      
      expected_batter <- 1 / (1 + 10^((r_p - r_b) / 400))
      actual_outcome <- 1 / (1 + exp(-slope * (g_perf - center_value)))
      
      delta <- K * (actual_outcome - expected_batter)
      batter_elos$current_elo[b_index] <- r_b + delta
      pitcher_elos$current_elo[p_index] <- r_p - delta
      
      loss_values <- c(loss_values, log_loss(actual_outcome, expected_batter))
    }
    
    return(mean(loss_values, na.rm = TRUE))
  }
  
  # 🔁 Tune across all slope values
  logloss_results <- sapply(slope_range, compute_logloss_for_slope)
  best_slope <- slope_range[which.min(logloss_results)]
  
  # 📈 Plot result
  plot(slope_range, logloss_results, type = "b", pch = 19,
       col = "darkgreen", main = "Logistic Slope Tuning via Log Loss",
       xlab = "Logistic Slope", ylab = "Log Loss")
  abline(v = best_slope, col = "red", lty = 2)
  
  cat(sprintf("✅ Optimal slope (min log loss): %.3f\n", best_slope))
  
  return(list(
    best_slope = best_slope,
    slope_range = slope_range,
    logloss_values = logloss_results,
    center_value = center_value
  ))
}


results_logloss <- tune_logistic_slope_logloss(
  BatterWithPitcher = BatterWithPitcher,
  batter_elos_raw = batter_elos,
  pitcher_elos_raw = pitcher_elos
)

optimal_slope = results_logloss$best_slope



center_value <- mean(BatterWithPitcher$GamePerformance, na.rm = TRUE)

logistic_score <- function(x, center = center_value, slope = optimal_slope) {
  1 / (1 + exp(-slope * (x - center)))
}

BatterWithPitcher <- BatterWithPitcher %>%
  mutate(performance_score = logistic_score(GamePerformance))


# === ELO SYSTEM === #
# --- Elo Update Setup ---
K <- 12.6
elo_log <- list()

# Copy elos to update during loop
elo_batters <- batter_elos %>% rename(batter_pk = player_pk)
elo_pitchers <- pitcher_elos %>% rename(pitcher_pk = player_pk)

total <- nrow(BatterWithPitcher)

# --- Elo Update Loop ---
for (i in 1:total) {
  row <- BatterWithPitcher[i, ]
  b_id <- row$batter_pk
  p_id <- row$pitcher_pk
  
  b_index <- match(b_id, elo_batters$batter_pk)
  p_index <- match(p_id, elo_pitchers$pitcher_pk)
  
  if (is.na(b_index) || is.na(p_index) || is.na(row$performance_score)) next
  
  b_rating <- elo_batters$current_elo[b_index]
  p_rating <- elo_pitchers$current_elo[p_index]
  outcome <- row$performance_score
  
  # Expected outcome for batter
  e_batter <- 1 / (1 + 10^((p_rating - b_rating) / 400))
  delta <- K * (outcome - e_batter)
  
  # Update ratings
  elo_batters$current_elo[b_index] <- b_rating + delta
  elo_pitchers$current_elo[p_index] <- p_rating - delta
  
  # Log result
  elo_log[[i]] <- data.frame(
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

# --- Output Log and Final Elo Ratings ---
EloLog <- bind_rows(elo_log)

FinalEloRatings <- bind_rows(
  elo_batters %>% rename(player_pk = batter_pk),
  elo_pitchers %>% rename(player_pk = pitcher_pk)
)

FinalEloWithNames <- FinalEloRatings %>%
  left_join(Rosters %>% select(player_pk, person_full_name, team_name, position_name),
            by = "player_pk") %>%
  distinct(player_pk, .keep_all = TRUE)




