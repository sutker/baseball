rm(list = ls())

# Load necessary libraries
library(baseballr)
library(dplyr)
library(readr)
library(lubridate)

# ================ #
# Team Information
# ================ #
teams <- teams_lu_table %>%
  filter(sport.name == "Major League Baseball")

teams_selected = teams %>%
  select(id, name, abbreviation, venue.id, venue.name, league.id, league.name, division.id, division.name)

# ================= #
# 2024 Team Rosters
# ================= #
# Initialize an empty data frame to store the full rosters
teams_roster <- data.frame()

# Loop through each team in teams_selected using the "id" column as team_id
for (i in 1:nrow(teams_selected)) {
  team_id <- teams_selected$id[i]  # Use "id" column for team_id
  team_name <- teams_selected$name[i]
  team_abbreviation <- teams_selected$abbreviation[i]
  
  cat("Fetching roster for:", team_name, "(", team_abbreviation, ")\n")
  
  # Retrieve the full roster for the given team_id
  tryCatch({
    roster <- mlb_rosters(team_id = team_id, season = 2024, roster_type = "fullRoster")
    
    # Add team information to the roster data frame
    roster$team_id <- team_id
    roster$team_name <- team_name
    roster$team_abbreviation <- team_abbreviation
    
    # Append to the main teams_roster data frame
    teams_roster <- bind_rows(teams_roster, roster)
    
    cat("✅ Successfully added roster for", team_name, "\n")
  }, error = function(e) {
    cat("❌ Error retrieving roster for", team_name, ":", conditionMessage(e), "\n")
  })
}

teams_roster_selected = teams_roster %>%
  select(
    season, 
    team_id, team_abbreviation, team_name, 
    person_id, person_full_name, position_name, position_abbreviation, status_description
  )

# Save to CSV for future use
write_csv(teams_roster_selected, "mlb_teams_roster_2024.csv")

cat("\n✅ All MLB team rosters collected and saved as 'mlb_teams_roster_2024.csv'.\n")


# ======================= #
# Team Statistics in 2024
# ======================= #
Team_FieldingStats = fg_team_fielder(startseason = 2024, endseason = 2024, qual = 'y')
Team_BattingStats = fg_team_batter(startseason = 2024, endseason = 2024, qual = 'y')
Team_PitchingStats = fg_team_pitcher(startseason = 2024, endseason = 2024, qual = 'y')


# ========================= #
# Player Statistics in 2024
# ========================= #
Player_FieldingStats = fg_fielder_leaders(startseason = 2024, endseason = 2024)
Player_BattingStats = fg_batter_leaders(startseason = 2024, endseason = 2024)

# ======================== #
# Game Information in 2024
# ======================== #

# Date Range
dates <- seq.Date(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
dates <- as.character(dates)  # Explicitly convert to character strings

# Initialize an empty list to store daily game data
game_info_list <- list()

# Loop through each date and fetch game data
instance_count <- 0  # Counter for tracking progress
total_iterations <- length(dates)  # Total number of dates

for (date in dates) {
  instance_count <- instance_count + 1  # Increment instance count
  cat("[", instance_count, "/", total_iterations, "] Fetching games for date:", date, "\n")
  
  tryCatch({
    df <- mlb_game_pks(date = date, level_ids = c(1))  # Pass properly formatted date
    
    if (is.data.frame(df) && nrow(df) > 0) {
      df <- df %>% mutate(game_date = date)  # Add date column
      game_info_list[[date]] <- df
      cat("✅ Data retrieved for", date, "- Rows:", nrow(df), "\n")
    } else {
      cat("⚠️ No games found for", date, "\n")
    }
  }, error = function(e) {
    cat("❌ Error fetching data for", date, ":", conditionMessage(e), "\n")
  })
}

# Combine all daily game data into a single data frame
game_info <- bind_rows(game_info_list)

# Selecting preferred columns
game_info_selected <- game_info %>%
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

cat("\n✅ Scraping complete. Collected game data for all of 2024.\n")


# ============================= #
# Batting & Pitching Statistics 
# ============================= #

baseball_data <- list()

# All player IDs who played in the 2024 season
all_players <- chadwick_player_lu() %>%
  filter(!is.na(key_fangraphs), mlb_played_last == 2024) %>%  # Filter players who played in 2024
  select(player_id = key_fangraphs)

# Define the range of years for which data should be collected
years <- 2024:2024  # Modify if needed

# Function to scrape batter game logs for ALL players in 2024
scrape_batter_game_logs <- function() {
  batter_logs <- list()
  instance_count <- 0  # Initialize counter
  
  total_iterations <- length(years) * nrow(all_players)  # Total expected iterations
  
  for (year in years) {
    cat("\n=== Scraping batter logs for year:", year, "===\n")
    
    for (player in all_players$player_id) {
      instance_count <- instance_count + 1  # Increment instance count
      cat("[", instance_count, "/", total_iterations, "] Processing PlayerID:", player, "Year:", year, "\n")
      
      tryCatch({
        df <- fg_batter_game_logs(playerid = player, year = year)
        
        if (is.data.frame(df) && nrow(df) > 0) {
          df <- df %>% mutate(player_id = player, year = year)  # Add player ID and year columns
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
  
  # Combine all lists into a single data frame
  batter_game_logs_df <- bind_rows(batter_logs)
  return(batter_game_logs_df)
}

scrape_pitcher_game_logs <- function() {
  pitcher_logs <- list()
  instance_count <- 0  # Initialize counter
  
  total_iterations <- length(years) * nrow(all_players)  # Total expected iterations
  
  for (year in years) {
    cat("\n=== Scraping pitcher logs for year:", year, "===\n")
    
    for (player in all_players$player_id) {
      instance_count <- instance_count + 1  # Increment instance count
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
  
  # Combine all lists into a single data frame
  pitcher_game_logs_df <- bind_rows(pitcher_logs)
  return(pitcher_game_logs_df)
}

# Execute function to collect batter logs into a data frame
baseball_data$batter_game_logs <- scrape_batter_game_logs()

# Execute function to collect pitcher logs into a data frame
baseball_data$pitcher_game_logs <- scrape_pitcher_game_logs()

# Define the export directory
export_dir <- "C:/Users/micha/OneDrive/Desktop/MLBData"

# Define file paths
batter_csv_path <- file.path(export_dir, "batter_game_logs_2024.csv")
pitcher_csv_path <- file.path(export_dir, "pitcher_game_logs_2024.csv")

# Save the final data frames as CSV files
write_csv(baseball_data$batter_game_logs, batter_csv_path)
write_csv(baseball_data$pitcher_game_logs, pitcher_csv_path)

# Save as an RData file for future use
save(baseball_data, file = file.path(export_dir, "full_baseball_database.RData"))

cat("\n✅ Scraping complete. Data saved as:\n")
cat("📂 Batter Logs: ", batter_csv_path, "\n")
cat("📂 Pitcher Logs: ", pitcher_csv_path, "\n")

# ============================================ #
# Description of Batting & Pitching Statistics
# ============================================ #

# Extract unique statistics from batter and pitcher game logs
batter_stats <- colnames(baseball_data$batter_game_logs)
pitcher_stats <- colnames(baseball_data$pitcher_game_logs)

# Combine and get unique statistics
all_stats <- unique(c(batter_stats, pitcher_stats))

# Initialize a lookup table with empty descriptions
stat_descriptions <- data.frame(
  Stat_Abbreviation = all_stats,
  Description = NA_character_,  # Empty descriptions to be manually filled
  stringsAsFactors = FALSE
)

# Manually define known descriptions for key stats
descriptions_list <- list(
  "PlayerName" = "Player's full name",
  "playerid" = "Unique player identifier",
  "Date" = "Date of the game",
  "Team" = "Player's team during the game",
  "Opp" = "Opponent team",
  "season" = "Season year",
  "Age" = "Player's age at the time of the game",
  "BatOrder" = "Batting order position",
  "Pos" = "Fielding position played",
  "G" = "Games played",
  "AB" = "At-bats",
  "PA" = "Plate appearances",
  "H" = "Total hits",
  "1B" = "Singles",
  "2B" = "Doubles",
  "3B" = "Triples",
  "HR" = "Home runs",
  "R" = "Runs scored",
  "RBI" = "Runs batted in",
  "BB" = "Walks",
  "IBB" = "Intentional walks",
  "SO" = "Strikeouts",
  "HBP" = "Hit by pitch",
  "SF" = "Sacrifice flies",
  "SH" = "Sacrifice hits",
  "GDP" = "Grounded into double play",
  "SB" = "Stolen bases",
  "CS" = "Caught stealing",
  "AVG" = "Batting average",
  "GB" = "Ground balls",
  "FB" = "Fly balls",
  "LD" = "Line drives",
  "IFFB" = "Infield fly balls",
  "Pitches" = "Total pitches seen",
  "Balls" = "Balls seen in the at-bat",
  "Strikes" = "Strikes seen in the at-bat",
  "IFH" = "Infield hits",
  "BU" = "Bunts attempted",
  "BUH" = "Bunt hits",
  "BB%" = "Walk percentage",
  "K%" = "Strikeout percentage",
  "BB/K" = "Walk-to-strikeout ratio",
  "OBP" = "On-base percentage",
  "SLG" = "Slugging percentage",
  "OPS" = "On-base plus slugging",
  "ISO" = "Isolated power",
  "BABIP" = "Batting average on balls in play",
  "GB/FB" = "Ground ball to fly ball ratio",
  "LD%" = "Line drive percentage",
  "GB%" = "Ground ball percentage",
  "FB%" = "Fly ball percentage",
  "IFFB%" = "Infield fly ball percentage",
  "HR/FB" = "Home runs per fly ball",
  "IFH%" = "Infield hit percentage",
  "BUH%" = "Bunt hit percentage",
  "wOBA" = "Weighted on-base average",
  "wRAA" = "Weighted runs above average",
  "wRC" = "Weighted runs created",
  "Spd" = "Baserunning speed score",
  "wRC+" = "Weighted runs created plus",
  "wBSR" = "Weighted baserunning runs",
  "WPA" = "Win probability added",
  "-WPA" = "Negative win probability added",
  "+WPA" = "Positive win probability added",
  "RE24" = "Run expectancy based on 24 base/out states",
  "REW" = "Run expectancy wins",
  "pLI" = "Pitch leverage index",
  "phLI" = "Pinch hit leverage index",
  "PH" = "Pinch hits",
  "WPA/LI" = "Win probability added per leverage index",
  "Clutch" = "Clutch performance measure",
  "FB%1" = "Fastball usage percentage",
  "FBv" = "Fastball velocity",
  "SL%" = "Slider usage percentage",
  "SLv" = "Slider velocity",
  "CT%" = "Cutter usage percentage",
  "CTv" = "Cutter velocity",
  "CB%" = "Curveball usage percentage",
  "CBv" = "Curveball velocity",
  "CH%" = "Changeup usage percentage",
  "CHv" = "Changeup velocity",
  "wFB" = "Weighted runs above average for fastballs",
  "wSL" = "Weighted runs above average for sliders",
  "wCT" = "Weighted runs above average for cutters",
  "wCB" = "Weighted runs above average for curveballs",
  "wCH" = "Weighted runs above average for changeups",
  "wFB/C" = "Weighted runs above average per 100 fastballs",
  "wSL/C" = "Weighted runs above average per 100 sliders",
  "wCT/C" = "Weighted runs above average per 100 cutters",
  "wCB/C" = "Weighted runs above average per 100 curveballs",
  "wCH/C" = "Weighted runs above average per 100 changeups",
  "O-Swing%" = "Outside swing percentage",
  "Z-Swing%" = "Zone swing percentage",
  "Swing%" = "Total swing percentage",
  "O-Contact%" = "Outside contact percentage",
  "Z-Contact%" = "Zone contact percentage",
  "Contact%" = "Total contact percentage",
  "Zone%" = "Percentage of pitches in the strike zone",
  "F-Strike%" = "First pitch strike percentage",
  "SwStr%" = "Swinging strike percentage",
  "Pull" = "Total pulled balls in play",
  "Cent" = "Total balls hit to center field",
  "Oppo" = "Total balls hit to opposite field",
  "Soft" = "Total soft contact balls",
  "Med" = "Total medium contact balls",
  "Hard" = "Total hard contact balls",
  "bipCount" = "Total balls in play",
  "Pull%" = "Percentage of pulled balls",
  "Cent%" = "Percentage of balls hit to center field",
  "Oppo%" = "Percentage of balls hit to opposite field",
  "Soft%" = "Soft contact percentage",
  "Med%" = "Medium contact percentage",
  "Hard%" = "Hard contact percentage",
  
  # Pitch Types - Frequency
  "pfxFA%" = "Percentage of fastballs thrown",
  "pfxFC%" = "Percentage of cutters thrown",
  "pfxSI%" = "Percentage of sinkers thrown",
  "pfxSL%" = "Percentage of sliders thrown",
  "pfxCU%" = "Percentage of curveballs thrown",
  "pfxCH%" = "Percentage of changeups thrown",
  
  # Pitch Velocity
  "pfxvFA" = "Velocity of fastballs (MPH)",
  "pfxvFC" = "Velocity of cutters (MPH)",
  "pfxvSI" = "Velocity of sinkers (MPH)",
  "pfxvSL" = "Velocity of sliders (MPH)",
  "pfxvCU" = "Velocity of curveballs (MPH)",
  "pfxvCH" = "Velocity of changeups (MPH)",
  
  # Pitch Movement - Horizontal
  "pfxFA-X" = "Horizontal movement of fastballs (inches)",
  "pfxFC-X" = "Horizontal movement of cutters (inches)",
  "pfxSI-X" = "Horizontal movement of sinkers (inches)",
  "pfxSL-X" = "Horizontal movement of sliders (inches)",
  "pfxCU-X" = "Horizontal movement of curveballs (inches)",
  "pfxCH-X" = "Horizontal movement of changeups (inches)",
  
  # Pitch Movement - Vertical
  "pfxFA-Z" = "Vertical movement of fastballs (inches)",
  "pfxFC-Z" = "Vertical movement of cutters (inches)",
  "pfxSI-Z" = "Vertical movement of sinkers (inches)",
  "pfxSL-Z" = "Vertical movement of sliders (inches)",
  "pfxCU-Z" = "Vertical movement of curveballs (inches)",
  "pfxCH-Z" = "Vertical movement of changeups (inches)",
  
  # Pitch Run Values
  "pfxwFA" = "Weighted runs above average for fastballs",
  "pfxwFC" = "Weighted runs above average for cutters",
  "pfxwSI" = "Weighted runs above average for sinkers",
  "pfxwSL" = "Weighted runs above average for sliders",
  "pfxwCU" = "Weighted runs above average for curveballs",
  "pfxwCH" = "Weighted runs above average for changeups",
  
  # Pitch Run Values per 100 Pitches
  "pfxwFA/C" = "Weighted runs above average per 100 fastballs",
  "pfxwFC/C" = "Weighted runs above average per 100 cutters",
  "pfxwSI/C" = "Weighted runs above average per 100 sinkers",
  "pfxwSL/C" = "Weighted runs above average per 100 sliders",
  "pfxwCU/C" = "Weighted runs above average per 100 curveballs",
  "pfxwCH/C" = "Weighted runs above average per 100 changeups",
  
  # Plate Discipline - Pitch f/x
  "pfxO-Swing%" = "Outside swing percentage (pitch f/x)",
  "pfxZ-Swing%" = "Zone swing percentage (pitch f/x)",
  "pfxSwing%" = "Total swing percentage (pitch f/x)",
  "pfxO-Contact%" = "Outside contact percentage (pitch f/x)",
  "pfxZ-Contact%" = "Zone contact percentage (pitch f/x)",
  "pfxContact%" = "Total contact percentage (pitch f/x)",
  "pfxZone%" = "Percentage of pitches in the strike zone (pitch f/x)",
  "pfxPace" = "Time between pitches (seconds)",
  
  # Pitcher Plate Discipline
  "piCH%" = "Percentage of changeups thrown by pitcher",
  "piCU%" = "Percentage of curveballs thrown by pitcher",
  "piFA%" = "Percentage of fastballs thrown by pitcher",
  "piFC%" = "Percentage of cutters thrown by pitcher",
  "piSI%" = "Percentage of sinkers thrown by pitcher",
  "piSL%" = "Percentage of sliders thrown by pitcher",
  
  # Pitcher Velocity
  "pivCH" = "Velocity of changeups by pitcher (MPH)",
  "pivCU" = "Velocity of curveballs by pitcher (MPH)",
  "pivFA" = "Velocity of fastballs by pitcher (MPH)",
  "pivFC" = "Velocity of cutters by pitcher (MPH)",
  "pivSI" = "Velocity of sinkers by pitcher (MPH)",
  "pivSL" = "Velocity of sliders by pitcher (MPH)",
  
  # Pitcher Movement - Horizontal
  "piCH-X" = "Horizontal movement of changeups by pitcher (inches)",
  "piCU-X" = "Horizontal movement of curveballs by pitcher (inches)",
  "piFA-X" = "Horizontal movement of fastballs by pitcher (inches)",
  "piFC-X" = "Horizontal movement of cutters by pitcher (inches)",
  "piSI-X" = "Horizontal movement of sinkers by pitcher (inches)",
  "piSL-X" = "Horizontal movement of sliders by pitcher (inches)",
  
  # Pitcher Movement - Vertical
  "piCH-Z" = "Vertical movement of changeups by pitcher (inches)",
  "piCU-Z" = "Vertical movement of curveballs by pitcher (inches)",
  "piFA-Z" = "Vertical movement of fastballs by pitcher (inches)",
  "piFC-Z" = "Vertical movement of cutters by pitcher (inches)",
  "piSI-Z" = "Vertical movement of sinkers by pitcher (inches)",
  "piSL-Z" = "Vertical movement of sliders by pitcher (inches)",
  
  # Pitcher Run Values
  "piwCH" = "Weighted runs above average for changeups by pitcher",
  "piwCU" = "Weighted runs above average for curveballs by pitcher",
  "piwFA" = "Weighted runs above average for fastballs by pitcher",
  "piwFC" = "Weighted runs above average for cutters by pitcher",
  "piwSI" = "Weighted runs above average for sinkers by pitcher",
  "piwSL" = "Weighted runs above average for sliders by pitcher",
  
  # Pitcher Run Values per 100 Pitches
  "piwCH/C" = "Weighted runs above average per 100 changeups by pitcher",
  "piwCU/C" = "Weighted runs above average per 100 curveballs by pitcher",
  "piwFA/C" = "Weighted runs above average per 100 fastballs by pitcher",
  "piwFC/C" = "Weighted runs above average per 100 cutters by pitcher",
  "piwSI/C" = "Weighted runs above average per 100 sinkers by pitcher",
  "piwSL/C" = "Weighted runs above average per 100 sliders by pitcher",
  
  # Plate Discipline - Pitcher
  "piO-Swing%" = "Outside swing percentage by pitcher",
  "piZ-Swing%" = "Zone swing percentage by pitcher",
  "piSwing%" = "Total swing percentage by pitcher",
  "piO-Contact%" = "Outside contact percentage by pitcher",
  "piZ-Contact%" = "Zone contact percentage by pitcher",
  "piContact%" = "Total contact percentage by pitcher",
  "piZone%" = "Percentage of pitches thrown in the strike zone (pitcher perspective)",
  "Events" = "Total number of events in a game",
  "EV" = "Exit velocity of a batted ball (MPH)",
  "LA" = "Launch angle of a batted ball (degrees)",
  "Barrels" = "Total number of barreled balls (optimal exit velocity and launch angle)",
  "Barrel%" = "Percentage of barreled balls",
  "maxEV" = "Maximum exit velocity in a game (MPH)",
  "HardHit" = "Total number of hard-hit balls",
  "HardHit%" = "Percentage of hard-hit balls",
  "gamedate" = "Date of the game",
  "dh" = "Indicates if a designated hitter was used",
  "player_id" = "Unique player identifier",
  "year" = "Season year",
  
  # Split-Finger Fastball (SF)
  "SF%" = "Splitter pitch usage percentage",
  "SFv" = "Splitter pitch velocity (MPH)",
  "wSF" = "Weighted runs above average for splitters",
  "wSF/C" = "Weighted runs above average per 100 splitters",
  
  # Forkball (FS) & Knuckle Curve (KC)
  "pfxFS%" = "Percentage of forkballs thrown",
  "pfxKC%" = "Percentage of knuckle-curves thrown",
  "pfxvFS" = "Velocity of forkballs (MPH)",
  "pfxvKC" = "Velocity of knuckle-curves (MPH)",
  "pfxFS-X" = "Horizontal movement of forkballs (inches)",
  "pfxKC-X" = "Horizontal movement of knuckle-curves (inches)",
  "pfxFS-Z" = "Vertical movement of forkballs (inches)",
  "pfxKC-Z" = "Vertical movement of knuckle-curves (inches)",
  "pfxwFS" = "Weighted runs above average for forkballs",
  "pfxwKC" = "Weighted runs above average for knuckle-curves",
  "pfxwFS/C" = "Weighted runs above average per 100 forkballs",
  "pfxwKC/C" = "Weighted runs above average per 100 knuckle-curves",
  
  # Pitcher Forkball (FS)
  "piFS%" = "Percentage of forkballs thrown by pitcher",
  "pivFS" = "Velocity of forkballs by pitcher (MPH)",
  "piFS-X" = "Horizontal movement of forkballs by pitcher",
  "piFS-Z" = "Vertical movement of forkballs by pitcher",
  "piwFS" = "Weighted runs above average for forkballs by pitcher",
  "piwFS/C" = "Weighted runs above average per 100 forkballs by pitcher",
  
  # Knuckleball (KN)
  "KN%" = "Knuckleball pitch usage percentage",
  "KNv" = "Knuckleball velocity (MPH)",
  "XX%" = "Unknown pitch type percentage",
  "wKN" = "Weighted runs above average for knuckleballs",
  "wKN/C" = "Weighted runs above average per 100 knuckleballs",
  
  # Eephus (EP) & Knuckleball (KN)
  "pfxEP%" = "Pitch frequency for eephus pitches",
  "pfxKN%" = "Pitch frequency for knuckleballs",
  "pfxvEP" = "Velocity of eephus pitches (MPH)",
  "pfxvKN" = "Velocity of knuckleballs (MPH)",
  "pfxEP-X" = "Horizontal movement of eephus pitches",
  "pfxKN-X" = "Horizontal movement of knuckleballs",
  "pfxEP-Z" = "Vertical movement of eephus pitches",
  "pfxKN-Z" = "Vertical movement of knuckleballs",
  "pfxwEP" = "Weighted runs above average for eephus pitches",
  "pfxwKN" = "Weighted runs above average for knuckleballs",
  
  # Pitch Run Values per 100 Pitches
  "pfxwEP/C" = "Weighted runs above average per 100 eephus pitches",
  "pfxwKN/C" = "Weighted runs above average per 100 knuckleballs",
  
  # Pitcher Knuckleball (KN)
  "piKN%" = "Percentage of knuckleballs thrown by pitcher",
  "pivKN" = "Velocity of knuckleballs by pitcher (MPH)",
  "piKN-X" = "Horizontal movement of knuckleballs by pitcher (inches)",
  "piKN-Z" = "Vertical movement of knuckleballs by pitcher (inches)",
  "piwKN" = "Weighted runs above average for knuckleballs by pitcher",
  "piwKN/C" = "Weighted runs above average per 100 knuckleballs by pitcher",
  
  # Pitcher Unknown Pitch (XX)
  "piXX%" = "Percentage of unknown pitch types thrown by pitcher",
  "pivXX" = "Velocity of unknown pitch types by pitcher (MPH)",
  "piXX-X" = "Horizontal movement of unknown pitch types by pitcher",
  "piXX-Z" = "Vertical movement of unknown pitch types by pitcher",
  "piwXX" = "Weighted runs above average for unknown pitch types",
  "piwXX/C" = "Weighted runs above average per 100 unknown pitch types",
  
  # Forkball (FO)
  "pfxFO%" = "Percentage of forkballs thrown",
  "pfxvFO" = "Velocity of forkballs (MPH)",
  "pfxFO-X" = "Horizontal movement of forkballs (inches)",
  "pfxFO-Z" = "Vertical movement of forkballs (inches)",
  "pfxwFO" = "Weighted runs above average for forkballs",
  "pfxwFO/C" = "Weighted runs above average per 100 forkballs",
  
  # Pitcher Cutter/Slider (CS)
  "piCS%" = "Percentage of cutters/sliders thrown by pitcher",
  "pivCS" = "Velocity of cutters/sliders by pitcher (MPH)",
  "piCS-X" = "Horizontal movement of cutters/sliders by pitcher",
  "piCS-Z" = "Vertical movement of cutters/sliders by pitcher",
  "piwCS" = "Weighted runs above average for cutters/sliders by pitcher",
  "piwCS/C" = "Weighted runs above average per 100 cutters/sliders by pitcher",
  
  # Screwball (SC)
  "pfxSC%" = "Percentage of screwballs thrown",
  "pfxvSC" = "Velocity of screwballs (MPH)",
  "pfxSC-X" = "Horizontal movement of screwballs (inches)",
  "pfxSC-Z" = "Vertical movement of screwballs (inches)",
  "pfxwSC" = "Weighted runs above average for screwballs",
  "pfxwSC/C" = "Weighted runs above average per 100 screwballs",
  
  # Pitcher Screwball (SC)
  "piSB%" = "Percentage of screwballs thrown by pitcher",
  "pivSB" = "Velocity of screwballs by pitcher (MPH)",
  "piSB-X" = "Horizontal movement of screwballs by pitcher",
  "piSB-Z" = "Vertical movement of screwballs by pitcher",
  "piwSB" = "Weighted runs above average for screwballs by pitcher",
  "piwSB/C" = "Weighted runs above average per 100 screwballs by pitcher",
  
  # Team Information
  "teamid" = "Unique team identifier",
  "HomeAway" = "Indicates if the team was home or away",
  
  # Pitching Statistics
  "W" = "Wins",
  "L" = "Losses",
  "ERA" = "Earned run average",
  "GS" = "Games started",
  "QS" = "Quality starts (min 6 innings, 3 or fewer earned runs)",
  "CG" = "Complete games",
  "ShO" = "Shutouts",
  "SV" = "Saves",
  "HLD" = "Holds",
  "BS" = "Blown saves",
  
  # Pitching Statistics
  "IP" = "Innings pitched",
  "TBF" = "Total batters faced",
  "ER" = "Earned runs allowed",
  "WP" = "Wild pitches",
  "BK" = "Balks",
  "K/9" = "Strikeouts per nine innings",
  "BB/9" = "Walks per nine innings",
  "H/9" = "Hits allowed per nine innings",
  "K/BB" = "Strikeouts-to-walks ratio",
  "K-BB%" = "Strikeout-minus-walk percentage",
  "SIERA" = "Skill-Interactive ERA",
  "HR/9" = "Home runs allowed per nine innings",
  "WHIP" = "Walks and hits per inning pitched",
  "LOB%" = "Left on base percentage",
  "FIP" = "Fielding Independent Pitching",
  "E-F" = "ERA minus FIP",
  "xFIP" = "Expected Fielding Independent Pitching",
  "ERA-" = "Park-adjusted ERA",
  "FIP-" = "Park-adjusted FIP",
  "xFIP-" = "Park-adjusted xFIP",
  "RS" = "Runs scored",
  "RS/9" = "Runs scored per nine innings",
  
  # Leverage & Game Impact Metrics
  "inLI" = "Leverage index when entering the game",
  "gmLI" = "Leverage index for the game",
  "Pulls" = "Total pulled hits",
  "Games" = "Total games played",
  "SD" = "Shutdowns (good relief appearances)",
  "MD" = "Meltdowns (bad relief appearances)",
  "tERA" = "True Earned Run Average",
  
  # Pitch Model Ratings
  "pb_o_FF" = "Pitch value for four-seam fastballs",
  "pb_s_FF" = "Stuff rating for four-seam fastballs",
  "pb_c_FF" = "Command rating for four-seam fastballs",
  "pb_overall" = "Overall pitcher rating",
  "pb_stuff" = "Pitcher's overall stuff rating",
  "pb_command" = "Pitcher's overall command rating",
  "pb_xRV100" = "Expected run value per 100 pitches",
  "pb_ERA" = "Pitcher's ERA based on pitch model",
  
  # Starting Pitcher Ratings
  "sp_stuff" = "Starter's stuff rating",
  "sp_location" = "Starter's location rating",
  "sp_pitching" = "Starter's overall pitching rating",
  
  # Expected Leverage Index
  "exLI" = "Expected leverage index",
  "GSv2" = "Games started, adjusted",
  
  # Pitch Model Ratings for Offspeed & Breaking Pitches
  "pb_o_CH" = "Pitch value for changeups",
  "pb_s_CH" = "Stuff rating for changeups",
  "pb_c_CH" = "Command rating for changeups",
  "pb_o_SL" = "Pitch value for sliders",
  "pb_s_SL" = "Stuff rating for sliders",
  "pb_c_SL" = "Command rating for sliders",
  "pb_o_FC" = "Pitch value for cutters",
  "pb_s_FC" = "Stuff rating for cutters",
  
  # Pitch Model Ratings for Cutters
  "pb_c_FC" = "Command rating for cutters",
  
  # Starter Ratings for Changeups
  "sp_s_CH" = "Stuff rating for changeups by starting pitchers",
  "sp_l_CH" = "Location rating for changeups by starting pitchers",
  "sp_p_CH" = "Overall pitching rating for changeups by starting pitchers",
  
  # Starter Ratings for Four-Seam Fastballs
  "sp_s_FF" = "Stuff rating for four-seam fastballs by starting pitchers",
  "sp_l_FF" = "Location rating for four-seam fastballs by starting pitchers",
  "sp_p_FF" = "Overall pitching rating for four-seam fastballs by starting pitchers",
  
  # Starter Ratings for Sliders
  "sp_s_SL" = "Stuff rating for sliders by starting pitchers",
  "sp_l_SL" = "Location rating for sliders by starting pitchers",
  "sp_p_SL" = "Overall pitching rating for sliders by starting pitchers",
  
  # Starter Ratings for Cutters
  "sp_s_FC" = "Stuff rating for cutters by starting pitchers",
  "sp_l_FC" = "Location rating for cutters by starting pitchers",
  "sp_p_FC" = "Overall pitching rating for cutters by starting pitchers",
  
  # Pitch Model Ratings for Sinkers
  "pb_o_SI" = "Pitch value for sinkers",
  "pb_s_SI" = "Stuff rating for sinkers",
  "pb_c_SI" = "Command rating for sinkers",
  
  # Starter Ratings for Sinkers
  "sp_s_SI" = "Stuff rating for sinkers by starting pitchers",
  "sp_l_SI" = "Location rating for sinkers by starting pitchers",
  "sp_p_SI" = "Overall pitching rating for sinkers by starting pitchers",
  
  # Pitch Model Ratings for Forkballs
  "pb_o_FS" = "Pitch value for forkballs",
  "pb_s_FS" = "Stuff rating for forkballs",
  "pb_c_FS" = "Command rating for forkballs",
  
  # Starter Ratings for Forkballs
  "sp_s_FS" = "Stuff rating for forkballs by starting pitchers",
  "sp_l_FS" = "Location rating for forkballs by starting pitchers",
  "sp_p_FS" = "Overall pitching rating for forkballs by starting pitchers",
  
  # Pitch Model Ratings for Curveballs
  "pb_o_CU" = "Pitch value for curveballs",
  "pb_s_CU" = "Stuff rating for curveballs",
  "pb_c_CU" = "Command rating for curveballs",
  
  # Starter Ratings for Curveballs
  "sp_s_CU" = "Stuff rating for curveballs by starting pitchers",
  "sp_l_CU" = "Location rating for curveballs by starting pitchers",
  "sp_p_CU" = "Overall pitching rating for curveballs by starting pitchers",
  
  # Pitch Model Ratings for Knuckle Curves
  "pb_o_KC" = "Pitch value for knuckle curves",
  "pb_s_KC" = "Stuff rating for knuckle curves",
  "pb_c_KC" = "Command rating for knuckle curves",
  
  # Starter Ratings for Knuckle Curves
  "sp_s_KC" = "Stuff rating for knuckle curves by starting pitchers",
  "sp_l_KC" = "Location rating for knuckle curves by starting pitchers",
  "sp_p_KC" = "Overall pitching rating for knuckle curves by starting pitchers",
  
  # Starter Ratings for Forkballs
  "sp_s_FO" = "Stuff rating for forkballs by starting pitchers",
  "sp_l_FO" = "Location rating for forkballs by starting pitchers",
  "sp_p_FO" = "Overall pitching rating for forkballs by starting pitchers"
)

# Fill descriptions dynamically
stat_descriptions$Description <- unlist(lapply(stat_descriptions$Stat_Abbreviation, function(stat) {
  if (stat %in% names(descriptions_list)) {
    return(descriptions_list[[stat]])
  } else {
    return(NA_character_)
  }
}))

# Save as CSV
write_csv(stat_descriptions, "mlb_statistics_descriptions.csv")

# Display the lookup table
print(stat_descriptions)

cat("\n✅ Lookup table updated and saved as 'mlb_statistics_lookup_complete.csv'.\n")
cat("Please review and fill in any missing descriptions manually.\n")

# ================== #
# Useful Data frames
# ================== #

View(stat_descriptions)

View(teams_selected)
View(teams_roster_selected)

View(Team_BattingStats)
View(Team_PitchingStats)
View(Team_FieldingStats)

View(Player_BattingStats)
View(Player_FieldingStats)

View(game_info_selected)
View(baseball_data$pitcher_game_logs)
View(baseball_data$batter_game_logs)
