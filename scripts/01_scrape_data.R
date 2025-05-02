# ==================================================================================== #
#                                     DATA COLLECTION
# ==================================================================================== #

# Clear environment
rm(list = ls())

# ===== Libraries =====
library(baseballr)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
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

# =================== #
# Historical Seasons
# =================== #
years <- 2021:2024

# Create empty lists to store data
TeamBattingStats <- list()
TeamPitchingStats <- list()
TeamFieldingStats <- list()
GameInfo <- list()

for (i in years) {
  cat("🔄 Collecting data for season:", i, "\n")
  
  # --- TEAM STATS ---
  TeamBattingStats[[as.character(i)]] <- fg_team_batter(startseason = i, endseason = i, qual = "y")
  assign(paste0("TeamBattingStatistics", i), TeamBattingStats[[as.character(i)]])
  
  TeamPitchingStats[[as.character(i)]] <- fg_team_pitcher(startseason = i, endseason = i, qual = "y")
  assign(paste0("TeamPitchingStatistics", i), TeamPitchingStats[[as.character(i)]])
  
  TeamFieldingStats[[as.character(i)]] <- fg_team_fielder(startseason = i, endseason = i, qual = "y")
  assign(paste0("TeamFieldingStatistics", i), TeamFieldingStats[[as.character(i)]])
  
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
  
  GameInfo[[as.character(i)]] <- bind_rows(game_info_list)
  
  assign(paste0("GameInformation", i), 
         GameInfo[[as.character(i)]] %>%
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
  )
}

# Combine all years into master game log
MasterGameInformation <- bind_rows(lapply(years, function(y) {
  get(paste0("GameInformation", y))
}))

cat("📦 Combined total games:", nrow(MasterGameInformation), "\n")

# =================== #
# 2025 Season Data
# =================== #
target_year <- 2025

# --- Team Statistics ---
TeamBattingStatistics2025 <- fg_team_batter(startseason = target_year, endseason = target_year, qual = "y")
TeamPitchingStatistics2025 <- fg_team_pitcher(startseason = target_year, endseason = target_year, qual = "y")
TeamFieldingStatistics2025 <- fg_team_fielder(startseason = target_year, endseason = target_year, qual = "y")

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

# --- Game Information 2025 ---
# Only fetch until current date to avoid errors
current_date <- as.Date("2025-04-29")  # Current date
dates_2025 <- seq.Date(as.Date("2025-03-01"), current_date, by = "day")
dates_2025 <- as.character(dates_2025)

game_info_list_2025 <- list()
instance_count <- 0
total_iterations <- length(dates_2025)

for (date in dates_2025) {
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

# --- Player Statistics 2025 ---
PlayerFieldingStatistics2025 <- fg_fielder_leaders(startseason = target_year, endseason = target_year)
PlayerBattingStatistics2025 <- fg_batter_leaders(startseason = target_year, endseason = target_year)

# --- Player Game Logs 2025 ---
# Get player IDs from Chadwick database
chadwick_raw <- chadwick_player_lu() 
chadwick_lookup <- chadwick_raw %>% 
  filter(!is.na(key_fangraphs)) %>% 
  select(mlb_id = key_mlbam, fangraphs_id = key_fangraphs)

# Add unified player_pk to Rosters2025
Rosters2025 <- Rosters2025 %>%
  left_join(chadwick_lookup, by = c("person_id" = "mlb_id")) %>%
  mutate(player_pk = ifelse(!is.na(fangraphs_id), fangraphs_id, person_id)) %>%
  select(season, team_id, team_abbreviation, team_name, person_id, person_full_name, 
         position_name, position_abbreviation, status_description, player_pk)

# Collect Batter and Pitcher Game Logs
all_players <- chadwick_player_lu() %>%
  filter(!is.na(key_fangraphs), mlb_played_last == target_year) %>%
  select(player_id = key_fangraphs)

# Batter Game Logs
BatterGameLogs2025 <- data.frame()
for (player in all_players$player_id) {
  tryCatch({
    df <- fg_batter_game_logs(playerid = player, year = target_year)
    if (is.data.frame(df) && nrow(df) > 0) {
      BatterGameLogs2025 <- bind_rows(BatterGameLogs2025, df)
      cat("✅ Added batter logs for player:", player, "\n")
    }
  }, error = function(e) {
    cat("❌ Error getting batter logs for player:", player, "\n")
  })
}

# Pitcher Game Logs
PitcherGameLogs2025 <- data.frame()
for (player in all_players$player_id) {
  tryCatch({
    df <- fg_pitcher_game_logs(playerid = player, year = target_year)
    if (is.data.frame(df) && nrow(df) > 0) {
      PitcherGameLogs2025 <- bind_rows(PitcherGameLogs2025, df)
      cat("✅ Added pitcher logs for player:", player, "\n")
    }
  }, error = function(e) {
    cat("❌ Error getting pitcher logs for player:", player, "\n")
  })
}

# Post-process game logs
BatterGameLogs2025 <- BatterGameLogs2025 %>%
  filter(AB > 0) %>%
  mutate(
    Home_Away = ifelse(startsWith(Opp, "@"), "Away", "Home"),
    HomeTeam = ifelse(startsWith(Opp, "@"), substr(Opp, 2, 4), Team),
    AwayTeam = ifelse(startsWith(Opp, "@"), Team, Opp),
    Date_Home_Away = paste(Date, HomeTeam, AwayTeam, sep = "_")
  ) %>%
  left_join(GameInformation2025 %>% select(Date_Home_Away, game_pk), by = "Date_Home_Away") %>%
  rename(player_pk = playerid)

PitcherGameLogs2025 <- PitcherGameLogs2025 %>%
  mutate(
    HomeTeam = ifelse(startsWith(Opp, "@"), substr(Opp, 2, 4), Team),
    AwayTeam = ifelse(startsWith(Opp, "@"), Team, Opp),
    Date_Home_Away = paste(Date, HomeTeam, AwayTeam, sep = "_")
  ) %>%
  left_join(GameInformation2025 %>% select(Date_Home_Away, game_pk), by = "Date_Home_Away") %>%
  rename(player_pk = playerid)

# Save data frames to RDS files in data folder
saveRDS(TeamBattingStatistics2021, "data/TeamBattingStatistics2021.rds")
saveRDS(TeamBattingStatistics2022, "data/TeamBattingStatistics2022.rds")
saveRDS(TeamBattingStatistics2023, "data/TeamBattingStatistics2023.rds")
saveRDS(TeamBattingStatistics2024, "data/TeamBattingStatistics2024.rds")
saveRDS(TeamBattingStatistics2025, "data/TeamBattingStatistics2025.rds")

saveRDS(TeamPitchingStatistics2021, "data/TeamPitchingStatistics2021.rds")
saveRDS(TeamPitchingStatistics2022, "data/TeamPitchingStatistics2022.rds")
saveRDS(TeamPitchingStatistics2023, "data/TeamPitchingStatistics2023.rds")
saveRDS(TeamPitchingStatistics2024, "data/TeamPitchingStatistics2024.rds")
saveRDS(TeamPitchingStatistics2025, "data/TeamPitchingStatistics2025.rds")

saveRDS(TeamFieldingStatistics2021, "data/TeamFieldingStatistics2021.rds")
saveRDS(TeamFieldingStatistics2022, "data/TeamFieldingStatistics2022.rds")
saveRDS(TeamFieldingStatistics2023, "data/TeamFieldingStatistics2023.rds")
saveRDS(TeamFieldingStatistics2024, "data/TeamFieldingStatistics2024.rds")
saveRDS(TeamFieldingStatistics2025, "data/TeamFieldingStatistics2025.rds")

saveRDS(MasterGameInformation, "data/MasterGameInformation.rds")
saveRDS(GameInformation2025, "data/GameInformation2025.rds")
saveRDS(Rosters2025, "data/Rosters2025.rds")
saveRDS(PlayerBattingStatistics2025, "data/PlayerBattingStatistics2025.rds")
saveRDS(PlayerFieldingStatistics2025, "data/PlayerFieldingStatistics2025.rds")
saveRDS(BatterGameLogs2025, "data/BatterGameLogs2025.rds")
saveRDS(PitcherGameLogs2025, "data/PitcherGameLogs2025.rds")
