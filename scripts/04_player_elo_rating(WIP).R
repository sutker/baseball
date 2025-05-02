# ==================================================================================== #
#                             PLAYER ELO SYSTEM IMPLEMENTATION
# ==================================================================================== #

# Clear environment
rm(list = ls())

# ===== Libraries =====
library(dplyr)
library(readr)

# ===== Load Data =====
BatterGameLogs2025 <- readRDS("data/BatterGameLogs2025.rds")
PitcherGameLogs2025 <- readRDS("data/PitcherGameLogs2025.rds")
Rosters2025 <- readRDS("data/Rosters2025.rds")
GameInformation2025 <- readRDS("data/GameInformation2025.rds")

# ===== Setup Logging =====
log_file <- "logs/script_04_output.log"
cat(format(Sys.time()), "Starting Player Elo System Implementation\n", file = log_file)

# ==================================================================================== #
#                               BATTER VS PITCHER MATCHUPS
# ==================================================================================== #

# === Identify Starting Pitchers ===
starting_pitchers <- PitcherGameLogs2025 %>%
  filter(GS == 1) %>%
  select(game_pk, pitcher_team = Team, pitcher_pk = player_pk)

cat("✅ Found", nrow(starting_pitchers), "starting pitcher appearances\n", file = log_file, append = TRUE)

# === Create Batter-Pitcher Matchup Data ===
Player_BatterWithPitcher <- BatterGameLogs2025 %>%
  select(game_pk, batter_pk = player_pk, Team, Date, `1B`, `2B`, `3B`, HR, BB, IBB, HBP, AB) %>%
  mutate(across(c(`1B`, `2B`, `3B`, HR, BB, IBB, HBP, AB), as.numeric)) %>%
  mutate(
    UBB = BB - IBB,  # Unintentional walks
    TotalBases = `1B` + 2 * `2B` + 3 * `3B` + 4 * HR,
    OnBaseEvents = UBB + HBP,
    GamePerformance = TotalBases + OnBaseEvents,
    GamePerformanceRate = ifelse(AB > 0, (TotalBases + OnBaseEvents) / AB, 0)
  ) %>%
  left_join(starting_pitchers, by = "game_pk") %>%
  filter(Team != pitcher_team)  # Remove cases where batter faces own team's pitcher

cat("✅ Created", nrow(Player_BatterWithPitcher), "batter-pitcher matchups\n", file = log_file, append = TRUE)

# ==================================================================================== #
#                               LOGISTIC SLOPE TUNING
# ==================================================================================== #

# === Initialize Player Elo Tables ===
Player_batter_elos <- Player_BatterWithPitcher %>%
  distinct(batter_pk) %>%
  mutate(current_elo = 1000)

Player_pitcher_elos <- Player_BatterWithPitcher %>%
  distinct(pitcher_pk) %>%
  rename(player_pk = pitcher_pk) %>%
  mutate(current_elo = 1000)

# === Slope Tuning Function ===
Player_tune_logistic_slope_logloss <- function(Player_BatterWithPitcher, Player_batter_elos_raw, Player_pitcher_elos_raw, 
                                             slope_range = seq(1.0, 100.0, by = 1), K = 50, verbose = TRUE) {
  # Calculate center value for logistic function
  Player_center_value <- mean(Player_BatterWithPitcher$GamePerformanceRate, na.rm = TRUE)
  if (verbose) cat(sprintf("📊 Logistic center (mean GamePerformanceRate): %.3f\n", Player_center_value), 
                  file = log_file, append = TRUE)
  
  # Log loss function
  log_loss <- function(actual, predicted, eps = 1e-15) {
    predicted <- pmin(pmax(predicted, eps), 1 - eps)
    -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
  }
  
  # Compute log loss for a given slope
  compute_logloss_for_slope <- function(slope) {
    Player_batter_elos <- Player_batter_elos_raw %>% 
      mutate(current_elo = 1000)
    
    Player_pitcher_elos <- Player_pitcher_elos_raw %>% 
      mutate(current_elo = 1000)
    
    loss_values <- c()
    
    for (i in seq_len(nrow(Player_BatterWithPitcher))) {
      row <- Player_BatterWithPitcher[i, ]
      
      if (is.na(row$batter_pk) || is.na(row$pitcher_pk) || is.na(row$GamePerformanceRate)) next
      
      b_index <- match(row$batter_pk, Player_batter_elos$batter_pk)
      p_index <- match(row$pitcher_pk, Player_pitcher_elos$pitcher_pk)
      
      if (is.na(b_index) || is.na(p_index)) next
      
      r_b <- Player_batter_elos$current_elo[b_index]
      r_p <- Player_pitcher_elos$current_elo[p_index]
      
      expected_batter <- 1 / (1 + 10^((r_p - r_b) / 400))
      actual_outcome <- 1 / (1 + exp(-slope * (row$GamePerformanceRate - Player_center_value)))
      
      delta <- K * (actual_outcome - expected_batter)
      Player_batter_elos$current_elo[b_index] <- r_b + delta
      Player_pitcher_elos$current_elo[p_index] <- r_p - delta
      
      loss_values <- c(loss_values, log_loss(actual_outcome, expected_batter))
    }
    
    mean(loss_values, na.rm = TRUE)
  }
  
  # Try different slopes
  logloss_results <- sapply(slope_range, compute_logloss_for_slope)
  best_slope <- slope_range[which.min(logloss_results)]
  
  cat(sprintf("✅ Optimal slope (min log loss): %.3f\n", best_slope), file = log_file, append = TRUE)
  
  return(list(
    best_slope = best_slope,
    slope_range = slope_range,
    logloss_values = logloss_results,
    Player_center_value = Player_center_value
  ))
}

# === Run Slope Tuning ===
Player_results_logloss <- Player_tune_logistic_slope_logloss(
  Player_BatterWithPitcher = Player_BatterWithPitcher,
  Player_batter_elos_raw = Player_batter_elos,
  Player_pitcher_elos_raw = Player_pitcher_elos,
  slope_range = seq(1.0, 100.0, by = 1),
  K = 50
)

Player_optimal_slope <- Player_results_logloss$best_slope
Player_center_value <- Player_results_logloss$Player_center_value

# ==================================================================================== #
#                               PLAYER ELO IMPLEMENTATION
# ==================================================================================== #

# === Logistic Score Function ===
Player_logistic_score <- function(x, center = Player_center_value, slope = Player_optimal_slope) {
  1 / (1 + exp(-slope * (x - center)))
}

# Add performance scores to matchup data
Player_BatterWithPitcher <- Player_BatterWithPitcher %>%
  mutate(performance_score = Player_logistic_score(GamePerformanceRate))

# === Initialize Elo System ===
K <- 50  # Elo K-factor
Player_elo_log <- list()

# Reinitialize Elo ratings
Player_elo_batters <- Player_BatterWithPitcher %>%
  distinct(batter_pk) %>%
  mutate(current_elo = 1000)

Player_elo_pitchers <- Player_BatterWithPitcher %>%
  distinct(pitcher_pk) %>%
  mutate(current_elo = 1000)

total_matchups <- nrow(Player_BatterWithPitcher)

# === Process Each Matchup ===
for (i in seq_len(total_matchups)) {
  row <- Player_BatterWithPitcher[i, ]
  b_id <- row$batter_pk
  p_id <- row$pitcher_pk
  
  b_index <- match(b_id, Player_elo_batters$batter_pk)
  p_index <- match(p_id, Player_elo_pitchers$pitcher_pk)
  
  if (is.na(b_index) || is.na(p_index) || is.na(row$performance_score)) next
  
  b_rating <- Player_elo_batters$current_elo[b_index]
  p_rating <- Player_elo_pitchers$current_elo[p_index]
  outcome <- row$performance_score
  
  # Calculate expected performance
  e_batter <- 1 / (1 + 10^((p_rating - b_rating) / 400))
  
  # Calculate Elo change
  delta <- K * (outcome - e_batter)
  
  # Update ratings
  Player_elo_batters$current_elo[b_index] <- b_rating + delta
  Player_elo_pitchers$current_elo[p_index] <- p_rating - delta
  
  # Log the update
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
  
  if (i %% 100 == 0 || i == total_matchups) {
    cat(sprintf("🔄 Processed %d of %d matchups (%.1f%%)\n", 
                i, total_matchups, 100 * i / total_matchups), 
        file = log_file, append = TRUE)
  }
}

# Combine all logs
PlayerEloLog2025 <- bind_rows(Player_elo_log)

# === Create Final Player Elo Ratings ===
PlayerFinalEloRatings2025 <- bind_rows(
  Player_elo_batters %>% rename(player_pk = batter_pk),
  Player_elo_pitchers %>% rename(player_pk = pitcher_pk)
)

# Add player information
PlayerFinalEloWithNames2025 <- PlayerFinalEloRatings2025 %>%
  left_join(
    Rosters2025 %>% 
      select(player_pk, person_full_name, team_name, position_name),
    by = "player_pk"
  ) %>%
  distinct(player_pk, .keep_all = TRUE)

# Print summary statistics
cat("\nPlayer Elo System Summary:\n", file = log_file, append = TRUE)
cat("Total batters processed:", length(unique(PlayerEloLog2025$batter_pk)), "\n", 
    file = log_file, append = TRUE)
cat("Total pitchers processed:", length(unique(PlayerEloLog2025$pitcher_pk)), "\n", 
    file = log_file, append = TRUE)
cat("Total matchups recorded:", nrow(PlayerEloLog2025), "\n", 
    file = log_file, append = TRUE)
cat("Elo range:", 
    round(min(PlayerFinalEloWithNames2025$current_elo)), "to",
    round(max(PlayerFinalEloWithNames2025$current_elo)), "\n", 
    file = log_file, append = TRUE)

# Save outputs
saveRDS(PlayerEloLog2025, "data/PlayerEloLog2025.rds")
saveRDS(PlayerFinalEloWithNames2025, "data/PlayerFinalEloWithNames2025.rds")

cat("\n✅ Player Elo System Implementation completed\n", file = log_file, append = TRUE)
