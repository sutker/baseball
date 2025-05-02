# ==================================================================================== #
#                             GAME OUTCOME PREDICTIONS
# ==================================================================================== #

# Clear environment
rm(list = ls())

# ===== Libraries =====
library(dplyr)
library(readr)
library(tidyr)
library(gbm)
library(randomForest)
library(scales)

# ===== Setup Logging =====
log_file <- "logs/script_05_output.log"
cat(format(Sys.time()), "Starting Game Outcome Predictions\n", file = log_file)

# ===== Load Data =====
# Load game information
MasterGameInformation <- readRDS("data/MasterGameInformation.rds")
GameInformation2025 <- readRDS("data/GameInformation2025.rds")

# Load Elo logs
TeamEloLog <- readRDS("data/TeamEloLog.rds")
TeamEloLog2025 <- readRDS("data/TeamEloLog2025.rds")

# Load optimization results
results_scaled <- readRDS("data/results_scaled.rds")

# ==================================================================================== #
#                             DATA PREPARATION
# ==================================================================================== #

# Team List
teams <- teams_lu_table %>%
  filter(sport.name == "Major League Baseball")

TeamList <- teams %>%
  select(id, name, bref_abbreviation, abbreviation, venue.id, venue.name, league.id, league.name, division.id, division.name)

# Team name abbreviation mapping for fielding stats
fielding_map <- c(
    "Diamondbacks" = "ARI", "Braves" = "ATL", "Orioles" = "BAL", "Red Sox" = "BOS", "Cubs" = "CHC",
    "White Sox" = "CHW", "Reds" = "CIN", "Guardians" = "CLE", "Rockies" = "COL", "Tigers" = "DET",
    "Astros" = "HOU", "Royals" = "KCR", "Angels" = "LAA", "Dodgers" = "LAD", "Marlins" = "MIA",
    "Brewers" = "MIL", "Twins" = "MIN", "Mets" = "NYM", "Yankees" = "NYY", "Athletics" = "OAK",
    "Phillies" = "PHI", "Pirates" = "PIT", "Padres" = "SDP", "Giants" = "SFG", "Mariners" = "SEA",
    "Cardinals" = "STL", "Rays" = "TBR", "Rangers" = "TEX", "Blue Jays" = "TOR", "Nationals" = "WSN"
)

# Helper function to merge stats
merge_stats <- function(data, stats, team_col, prefix) {
    stats_renamed <- stats %>%
        rename_with(~ paste0(prefix, "_", .), .cols = !matches("team_name"))
    data %>%
        left_join(stats_renamed, by = setNames("team_name", team_col))
}

# Build GameData for historical seasons (2021-2024)
cat("Building historical GameData (2021-2024)...\n", file = log_file, append = TRUE)

all_GameData <- list()
years <- 2021:2024

for (year in years) {
    cat(sprintf("Processing year %d...\n", year), file = log_file, append = TRUE)
    
    games_year <- MasterGameInformation %>%
        filter(season == year) %>%
        rename(
            home_runs = teams.home.score,
            away_runs = teams.away.score
        )
    
    # Load and process batting stats
    batting_stats <- readRDS(sprintf("data/TeamBattingStatistics%d.rds", year)) %>%
        mutate(
            wRC = wRC / G,
            R = R / G
        ) %>%
        select(team_name, OBP, SLG, AVG, wRC, R)
    
    # Load and process pitching stats
    pitching_stats <- readRDS(sprintf("data/TeamPitchingStatistics%d.rds", year)) %>%
        select(team_name, ERA, WHIP, K_9, BB_9, HR_9)
    
    # Load and process fielding stats
    fielding_stats <- readRDS(sprintf("data/TeamFieldingStatistics%d.rds", year)) %>%
        mutate(
            team_name = fielding_map[team_name],
            DRS = DRS / G,
            Defense = Defense / G
        ) %>%
        filter(!is.na(team_name)) %>%
        select(team_name, DRS, Defense)
    
    # Merge Elo ratings
    games_year <- games_year %>%
        left_join(TeamEloLog %>% 
                     filter(date >= as.Date(paste0(year, "-01-01")) &
                              date <= as.Date(paste0(year, "-12-31"))) %>%
                     select(game_pk, home_elo_before, away_elo_before), 
                 by = "game_pk")
    
    # Merge all stats
    games_year <- games_year %>%
        merge_stats(batting_stats, "home_team_abbr", "home") %>%
        merge_stats(pitching_stats, "home_team_abbr", "home") %>%
        merge_stats(fielding_stats, "home_team_abbr", "home") %>%
        merge_stats(batting_stats, "away_team_abbr", "away") %>%
        merge_stats(pitching_stats, "away_team_abbr", "away") %>%
        merge_stats(fielding_stats, "away_team_abbr", "away")
    
    all_GameData[[as.character(year)]] <- games_year
}

# Combine all years
GameData <- bind_rows(all_GameData)

# Build TrainingData for historical seasons
TrainingData <- GameData %>%
    select(
        game_pk, officialDate, season, home_team_abbr, away_team_abbr,
        home_elo_before, away_elo_before, home_runs, away_runs,
        starts_with("home_"), starts_with("away_")
    )

# Clean data and split into train/test
set.seed(42)
TrainingDataClean <- TrainingData %>% na.omit()
rows <- nrow(TrainingDataClean)
train_indices <- sample(1:rows, size = 0.8 * rows)

train_data <- TrainingDataClean[train_indices, ]
test_data <- TrainingDataClean[-train_indices, ]

# ==================================================================================== #
#                             MODEL TRAINING AND EVALUATION
# ==================================================================================== #

cat("Training models...\n", file = log_file, append = TRUE)

# === LINEAR REGRESSION === #
cat("Training Linear Regression models...\n", file = log_file, append = TRUE)

# Home team
lm_home <- lm(home_runs ~ home_elo_before + away_elo_before +
                home_OBP + home_SLG + home_AVG + home_wRC + home_R +
                home_ERA + home_WHIP + home_K_9 + home_BB_9 + home_HR_9 +
                home_DRS + home_Defense +
                away_ERA + away_WHIP + away_K_9 + away_BB_9 + away_HR_9 +
                away_DRS + away_Defense,
              data = train_data)

home_preds_lm <- predict(lm_home, newdata = test_data)
home_rmse_lm <- sqrt(mean((test_data$home_runs - home_preds_lm)^2))
cat(sprintf("Linear Regression Home RMSE: %.3f\n", home_rmse_lm), file = log_file, append = TRUE)

# Away team
lm_away <- lm(away_runs ~ away_elo_before + home_elo_before +
                away_OBP + away_SLG + away_AVG + away_wRC + away_R +
                away_ERA + away_WHIP + away_K_9 + away_BB_9 + away_HR_9 +
                away_DRS + away_Defense +
                home_ERA + home_WHIP + home_K_9 + home_BB_9 + home_HR_9 +
                home_DRS + home_Defense,
              data = train_data)

away_preds_lm <- predict(lm_away, newdata = test_data)
away_rmse_lm <- sqrt(mean((test_data$away_runs - away_preds_lm)^2))
cat(sprintf("Linear Regression Away RMSE: %.3f\n", away_rmse_lm), file = log_file, append = TRUE)

# === GBM === #
cat("Training GBM models...\n", file = log_file, append = TRUE)

# Home team
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
cat(sprintf("GBM Home RMSE: %.3f\n", home_rmse_gbm), file = log_file, append = TRUE)

# Away team
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
cat(sprintf("GBM Away RMSE: %.3f\n", away_rmse_gbm), file = log_file, append = TRUE)

# === POISSON REGRESSION === #
cat("Training Poisson Regression models...\n", file = log_file, append = TRUE)

# Home team
glm_home <- glm(home_runs ~ home_elo_before + away_elo_before +
                  home_OBP + home_SLG + home_AVG + home_wRC + home_R +
                  home_ERA + home_WHIP + home_K_9 + home_BB_9 + home_HR_9 +
                  away_ERA + away_WHIP + away_K_9 + away_BB_9 + away_HR_9,
                family = "poisson", data = train_data)

home_preds_poisson <- predict(glm_home, newdata = test_data, type = "response")
home_rmse_poisson <- sqrt(mean((test_data$home_runs - home_preds_poisson)^2))
cat(sprintf("Poisson Home RMSE: %.3f\n", home_rmse_poisson), file = log_file, append = TRUE)

# Away team
glm_away <- glm(away_runs ~ away_elo_before + home_elo_before +
                  away_OBP + away_SLG + away_AVG + away_wRC + away_R +
                  away_ERA + away_WHIP + away_K_9 + away_BB_9 + away_HR_9 +
                  home_ERA + home_WHIP + home_K_9 + home_BB_9 + home_HR_9,
                family = "poisson", data = train_data)

away_preds_poisson <- predict(glm_away, newdata = test_data, type = "response")
away_rmse_poisson <- sqrt(mean((test_data$away_runs - away_preds_poisson)^2))
cat(sprintf("Poisson Away RMSE: %.3f\n", away_rmse_poisson), file = log_file, append = TRUE)

# === RANDOM FOREST === #
cat("Training Random Forest models...\n", file = log_file, append = TRUE)

# Home team
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
cat(sprintf("Random Forest Home RMSE: %.3f\n", rf_rmse_home), file = log_file, append = TRUE)

# Away team
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
cat(sprintf("Random Forest Away RMSE: %.3f\n", rf_rmse_away), file = log_file, append = TRUE)

# ==================================================================================== #
#                             MODEL SELECTION AND PREDICTION
# ==================================================================================== #

# Compare models and select best by RMSE
model_rmse <- c(
    poisson_home = home_rmse_poisson,
    poisson_away = away_rmse_poisson,
    linear_home = home_rmse_lm,
    linear_away = away_rmse_lm,
    gbm_home = home_rmse_gbm,
    gbm_away = away_rmse_gbm,
    rf_home = rf_rmse_home,
    rf_away = rf_rmse_away
)

home_models <- model_rmse[c("poisson_home", "linear_home", "gbm_home", "rf_home")]
away_models <- model_rmse[c("poisson_away", "linear_away", "gbm_away", "rf_away")]

best_home_model <- names(home_models)[which.min(home_models)]
best_away_model <- names(away_models)[which.min(away_models)]

cat(sprintf("Best home model: %s (RMSE: %.3f)\n", best_home_model, min(home_models)), 
    file = log_file, append = TRUE)
cat(sprintf("Best away model: %s (RMSE: %.3f)\n", best_away_model, min(away_models)), 
    file = log_file, append = TRUE)

# Apply best models to historical data
TrainingDataClean <- TrainingDataClean %>%
    mutate(
        predicted_home_runs = case_when(
            best_home_model == "poisson_home" ~ predict(glm_home, newdata = TrainingDataClean, type = "response"),
            best_home_model == "linear_home" ~ predict(lm_home, newdata = TrainingDataClean),
            best_home_model == "gbm_home" ~ predict(gbm_home, newdata = TrainingDataClean, n.trees = best_iter_home),
            best_home_model == "rf_home" ~ predict(rf_home, newdata = TrainingDataClean)
        ),
        predicted_away_runs = case_when(
            best_away_model == "poisson_away" ~ predict(glm_away, newdata = TrainingDataClean, type = "response"),
            best_away_model == "linear_away" ~ predict(lm_away, newdata = TrainingDataClean),
            best_away_model == "gbm_away" ~ predict(gbm_away, newdata = TrainingDataClean, n.trees = best_iter_away),
            best_away_model == "rf_away" ~ predict(rf_away, newdata = TrainingDataClean)
        )
    )

# ==================================================================================== #
#                             2025 SEASON PREDICTIONS
# ==================================================================================== #

cat("Processing 2025 season data...\n", file = log_file, append = TRUE)

# Build GameData for 2025
games_2025 <- GameInformation2025 %>%
    rename(
        home_runs = teams.home.score,
        away_runs = teams.away.score
    )

# Load 2025 team stats
batting_stats_2025 <- readRDS("data/TeamBattingStatistics2025.rds") %>%
    mutate(
        wRC = wRC / G,
        R = R / G
    ) %>%
    select(team_name, OBP, SLG, AVG, wRC, R)

pitching_stats_2025 <- readRDS("data/TeamPitchingStatistics2025.rds") %>%
    select(team_name, ERA, WHIP, K_9, BB_9, HR_9)

fielding_stats_2025 <- readRDS("data/TeamFieldingStatistics2025.rds") %>%
    mutate(
        team_name = fielding_map[team_name],
        DRS = DRS / G,
        Defense = Defense / G
    ) %>%
    filter(!is.na(team_name)) %>%
    select(team_name, DRS, Defense)

# Merge Elo ratings and stats for 2025
games_2025 <- games_2025 %>%
    left_join(TeamEloLog2025 %>% select(game_pk, home_elo_before, away_elo_before),
              by = "game_pk") %>%
    merge_stats(batting_stats_2025, "home_team_abbr", "home") %>%
    merge_stats(pitching_stats_2025, "home_team_abbr", "home") %>%
    merge_stats(fielding_stats_2025, "home_team_abbr", "home") %>%
    merge_stats(batting_stats_2025, "away_team_abbr", "away") %>%
    merge_stats(pitching_stats_2025, "away_team_abbr", "away") %>%
    merge_stats(fielding_stats_2025, "away_team_abbr", "away")

# Create training data for 2025
TrainingData2025 <- games_2025 %>%
    select(
        game_pk, officialDate, season, home_team_abbr, away_team_abbr,
        home_elo_before, away_elo_before, home_runs, away_runs,
        starts_with("home_"), starts_with("away_")
    )

# Clean and prepare 2025 data
TrainingDataClean2025 <- TrainingData2025 %>% na.omit()

# Apply best models to 2025 data
TrainingDataClean2025 <- TrainingDataClean2025 %>%
    mutate(
        predicted_home_runs = case_when(
            best_home_model == "poisson_home" ~ predict(glm_home, newdata = TrainingDataClean2025, type = "response"),
            best_home_model == "linear_home" ~ predict(lm_home, newdata = TrainingDataClean2025),
            best_home_model == "gbm_home" ~ predict(gbm_home, newdata = TrainingDataClean2025, n.trees = best_iter_home),
            best_home_model == "rf_home" ~ predict(rf_home, newdata = TrainingDataClean2025)
        ),
        predicted_away_runs = case_when(
            best_away_model == "poisson_away" ~ predict(glm_away, newdata = TrainingDataClean2025, type = "response"),
            best_away_model == "linear_away" ~ predict(lm_away, newdata = TrainingDataClean2025),
            best_away_model == "gbm_away" ~ predict(gbm_away, newdata = TrainingDataClean2025, n.trees = best_iter_away),
            best_away_model == "rf_away" ~ predict(rf_away, newdata = TrainingDataClean2025)
        )
    )

# ==================================================================================== #
#                             WIN PROBABILITY MODELS
# ==================================================================================== #

cat("Training win probability models...\n", file = log_file, append = TRUE)

# Prepare data for win probability modeling
TrainingDataClean <- TrainingDataClean %>%
    mutate(
        actual_winner = ifelse(home_runs > away_runs, 1, 0),
        predicted_diff = predicted_home_runs - predicted_away_runs
    )

TrainingDataClean2025 <- TrainingDataClean2025 %>%
    mutate(
        actual_winner = ifelse(home_runs > away_runs, 1, 0),
        predicted_diff = predicted_home_runs - predicted_away_runs
    )

# Logistic regression model
logit_win_model <- glm(actual_winner ~ predicted_diff, 
                       data = TrainingDataClean, 
                       family = "binomial")

# Pythagorean expectation tuning
exponents <- c(1.83, 2)
pythag_accuracy <- numeric(length(exponents))

for (i in seq_along(exponents)) {
    exp_val <- exponents[i]
    valid <- TrainingDataClean %>%
        filter(predicted_home_runs > 0, predicted_away_runs > 0)
    
    probs <- (valid$predicted_home_runs^exp_val) / 
        ((valid$predicted_home_runs^exp_val) + (valid$predicted_away_runs^exp_val))
    
    preds <- ifelse(probs > 0.5, 1, 0)
    pythag_accuracy[i] <- mean(preds == valid$actual_winner)
}

# Find best model
best_exp <- exponents[which.max(pythag_accuracy)]
best_pythag_acc <- max(pythag_accuracy)

# Test logistic regression accuracy
TrainingDataClean$win_prob_logistic <- predict(logit_win_model, 
                                              newdata = TrainingDataClean, 
                                              type = "response")
TrainingDataClean$pred_logistic_winner <- ifelse(TrainingDataClean$win_prob_logistic > 0.5, 1, 0)
accuracy_logistic <- mean(TrainingDataClean$pred_logistic_winner == TrainingDataClean$actual_winner)

# Choose best method
use_logistic <- accuracy_logistic >= best_pythag_acc
cat(sprintf("Using %s for win probability\n", 
            ifelse(use_logistic, "Logistic Regression", "Pythagorean Expectation")),
    file = log_file, append = TRUE)

# Update TeamEloLog with predictions
predictions <- TrainingDataClean %>%
    select(game_pk, predicted_home_runs, predicted_away_runs) %>%
    mutate(
        predicted_diff = predicted_home_runs - predicted_away_runs
    )

TeamEloLog <- TeamEloLog %>%
    filter(home_team_name != "Cleveland Indians",
            away_team_name != "Cleveland Indians")%>% ###### FIX CLEVELAND INDIANS TO GUARDIANS ###### & Athletics...
    left_join(predictions, by = "game_pk")%>%
    mutate(
        home_win_probability = if (use_logistic) {
            predict(logit_win_model, newdata = ., type = "response")
        } else {
            (predicted_home_runs^best_exp) / 
                ((predicted_home_runs^best_exp) + (predicted_away_runs^best_exp))
        },
        away_win_probability = 1 - home_win_probability
    )

opponent_strength <- TeamEloLog %>%
  transmute(team_id = home_team_id, opponent_elo = away_elo_before) %>%
  bind_rows(
    TeamEloLog %>%
      transmute(team_id = away_team_id, opponent_elo = home_elo_before)
  ) %>%
  group_by(team_id) %>%
  summarise(avg_opp_elo = mean(opponent_elo), .groups = "drop")

# Update TeamEloLog2025 with predictions
predictions2025 <- TrainingDataClean2025 %>%
    select(game_pk, predicted_home_runs, predicted_away_runs)%>%
    mutate(
        predicted_diff = predicted_home_runs - predicted_away_runs
    )

TeamEloLog2025 <- TeamEloLog2025 %>%
    left_join(predictions2025, by = "game_pk") %>%
    mutate(
        home_win_probability = if (use_logistic) {
            predict(logit_win_model, newdata = ., type = "response")
        } else {
            (predicted_home_runs^best_exp) / 
                ((predicted_home_runs^best_exp) + (predicted_away_runs^best_exp))
        },
        away_win_probability = 1 - home_win_probability
    )

opponent_strength_2025 <- TeamEloLog2025 %>%
  transmute(team_id = home_team_id, opponent_elo = away_elo_before) %>%
  bind_rows(
    TeamEloLog2025 %>%
      transmute(team_id = away_team_id, opponent_elo = home_elo_before)
  ) %>%
  group_by(team_id) %>%
  summarise(avg_opp_elo = mean(opponent_elo), .groups = "drop")

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

# Save updated datasets
saveRDS(TrainingDataClean, "data/TrainingDataClean.rds")
saveRDS(TrainingDataClean2025, "data/TrainingDataClean2025.rds")
saveRDS(TeamEloLog, "data/TeamEloLog.rds")
saveRDS(TeamEloLog2025, "data/TeamEloLog2025.rds")
saveRDS(TeamFinalSummary, "data/TeamFinalSummary.rds")
saveRDS(TeamFinalSummary2025, "data/TeamFinalSummary2025.rds")

cat("\n✅ Script completed successfully\n", file = log_file, append = TRUE)

glm_home_coefficients <- data.frame(
    variable = names(coef(glm_home)),
    coefficient = coef(glm_home)
)

glm_away_coefficients <- data.frame(
    variable = names(coef(glm_away)),
    coefficient = coef(glm_away)
)

saveRDS(glm_home_coefficients, "data/glm_home_coefficients.rds")
saveRDS(glm_away_coefficients, "data/glm_away_coefficients.rds")

