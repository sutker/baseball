# ==================================================================================== #
#                             MODEL ACCURACY AND FEATURE IMPORTANCE ANALYSIS
# ==================================================================================== #

# Clear environment
rm(list = ls())

# ====== Libraries =====
library(dplyr)
library(ggplot2)
library(scales)
library(caret)
library(xgboost)
library(SHAPforxgboost)
library(pROC)

# Define features used in the model
features <- c("OBP_diff", "SLG_diff", "ISO_diff", "wOBA_diff", "wRC_diff", "WAR_diff",
             "ERA_diff", "FIP_diff", "Kpct_diff", "BBpct_diff", "Elo_diff", "HomeFieldAdvantage")

# ===== Setup Logging =====
log_file <- "logs/script_07_output.log"
cat(format(Sys.time()), "Starting Model Analysis\n", file = log_file)

# ===== Load Data =====
# Load game information
MasterGameInformation <- readRDS("data/MasterGameInformation.rds")
GameInformation2025 <- readRDS("data/GameInformation2025.rds")

# Load Elo logs
TeamEloLog <- readRDS("data/TeamEloLog.rds")
TeamEloLog2025 <- readRDS("data/TeamEloLog2025.rds")

# Load optimization results
results_scaled <- readRDS("data/results_scaled.rds")

# Function to load and process team statistics for a given year
load_team_stats <- function(year) {
    batting <- readRDS(sprintf("data/TeamBattingStatistics%d.rds", year)) %>%
        mutate(
            WAR = WAR / G,  # Normalize WAR by games played
            wRC = wRC / G   # Normalize wRC by games played
        ) %>%
        select(team_name, OBP, SLG, ISO, wOBA, wRC, WAR)
    
    pitching <- readRDS(sprintf("data/TeamPitchingStatistics%d.rds", year)) %>%
        select(team_name, ERA, FIP, K_pct, BB_pct)
    
    list(batting = batting, pitching = pitching)
}

# Load historical seasons (2021-2024)
years <- 2021:2024
historical_stats <- lapply(years, load_team_stats)

# Load 2025 stats
stats_2025 <- load_team_stats(2025)

# Helper function to merge stats
merge_and_diff <- function(game_data, batting_stats, pitching_stats) {
    game_data %>%
        left_join(batting_stats, by = c("home_team_abbr" = "team_name")) %>%
        rename_with(~paste0("home_", .), .cols = c(OBP, SLG, ISO, wOBA, wRC, WAR)) %>%
        left_join(batting_stats, by = c("away_team_abbr" = "team_name")) %>%
        rename_with(~paste0("away_", .), .cols = c(OBP, SLG, ISO, wOBA, wRC, WAR)) %>%
        left_join(pitching_stats, by = c("home_team_abbr" = "team_name")) %>%
        rename_with(~paste0("home_", .), .cols = c(ERA, FIP, K_pct, BB_pct)) %>%
        left_join(pitching_stats, by = c("away_team_abbr" = "team_name")) %>%
        rename_with(~paste0("away_", .), .cols = c(ERA, FIP, K_pct, BB_pct)) %>%
        mutate(
            OBP_diff = home_OBP - away_OBP,
            SLG_diff = home_SLG - away_SLG,
            ISO_diff = home_ISO - away_ISO,
            wOBA_diff = home_wOBA - away_wOBA,
            wRC_diff = home_wRC - away_wRC,
            WAR_diff = home_WAR - away_WAR,
            ERA_diff = home_ERA - away_ERA,
            FIP_diff = home_FIP - away_FIP,
            Kpct_diff = home_K_pct - away_K_pct,
            BBpct_diff = home_BB_pct - away_BB_pct,
            Elo_diff = home_elo_before - away_elo_before,
            HomeFieldAdvantage = 1
        )
}

# Process historical data
GameData <- data.frame()
for (i in seq_along(years)) {
    year_games <- MasterGameInformation %>%
        filter(season == years[i]) %>%
        left_join(TeamEloLog %>% select(game_pk, home_elo_before, away_elo_before),
                 by = "game_pk")
    
    year_data <- merge_and_diff(year_games, 
                               historical_stats[[i]]$batting,
                               historical_stats[[i]]$pitching)
    GameData <- bind_rows(GameData, year_data)
}

# Process 2025 data
GameData2025 <- GameInformation2025 %>%
    left_join(TeamEloLog2025 %>% select(game_pk, home_elo_before, away_elo_before),
              by = "game_pk") %>%
    merge_and_diff(., stats_2025$batting, stats_2025$pitching)

# Prepare features and target for historical data
clean_data <- GameData %>% 
    mutate(actual_winner = ifelse(teams.home.score > teams.away.score, 1, 0)) %>%
    select(all_of(c(features, "actual_winner"))) %>% 
    na.omit()

# Prepare features and target for 2025 data
clean_data_2025 <- GameData2025 %>%
    mutate(actual_winner = ifelse(teams.home.score > teams.away.score, 1, 0)) %>%
    select(all_of(c(features, "actual_winner"))) %>%
    na.omit()

# Load model
xgb_model <- readRDS("data/xgboost_final_model.rds")

# Calculate predictions
historical_predictions <- data.frame(
    actual = clean_data$actual_winner,
    predicted_prob = predict(xgb_model, as.matrix(clean_data[, features]))
)

# Calculate ROC curve and AUC
roc_curve <- roc(historical_predictions$actual, historical_predictions$predicted_prob)
auc_score <- auc(roc_curve)

# Create ROC plot
roc_plot <- ggroc(roc_curve) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    labs(
        title = "ROC Curve for Historical Games (2021-2024)",
        subtitle = sprintf("AUC = %.3f", auc_score)
    ) +
    theme_minimal()

# Calculate accuracy metrics
predicted_outcomes <- ifelse(historical_predictions$predicted_prob > 0.5, 1, 0)
confusion_matrix <- confusionMatrix(factor(predicted_outcomes), factor(historical_predictions$actual))

# Log accuracy metrics
cat("\nHistorical Model Performance (2021-2024):\n", file = log_file, append = TRUE)
cat(sprintf("Accuracy: %.3f\n", confusion_matrix$overall["Accuracy"]), file = log_file, append = TRUE)
cat(sprintf("AUC: %.3f\n", auc_score), file = log_file, append = TRUE)
cat("\nConfusion Matrix:\n", file = log_file, append = TRUE)
print(confusion_matrix$table, file = log_file, append = TRUE)

# ==================================================================================== #
#                             2025 MODEL ACCURACY
# ==================================================================================== #

# Calculate predictions for 2025
predictions_2025 <- data.frame(
    actual = clean_data_2025$actual_winner,
    predicted_prob = predict(xgb_model, as.matrix(clean_data_2025[, features]))
)

# Calculate ROC curve and AUC for 2025
roc_curve_2025 <- roc(predictions_2025$actual, predictions_2025$predicted_prob)
auc_score_2025 <- auc(roc_curve_2025)

# Create ROC plot for 2025
roc_plot_2025 <- ggroc(roc_curve_2025) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    labs(
        title = "ROC Curve for 2025 Season",
        subtitle = sprintf("AUC = %.3f", auc_score_2025)
    ) +
    theme_minimal()

# Calculate accuracy metrics for 2025
predicted_outcomes_2025 <- ifelse(predictions_2025$predicted_prob > 0.5, 1, 0)
confusion_matrix_2025 <- confusionMatrix(factor(predicted_outcomes_2025), factor(predictions_2025$actual))

# Log 2025 accuracy metrics
cat("\n2025 Model Performance:\n", file = log_file, append = TRUE)
cat(sprintf("Accuracy: %.3f\n", confusion_matrix_2025$overall["Accuracy"]), file = log_file, append = TRUE)
cat(sprintf("AUC: %.3f\n", auc_score_2025), file = log_file, append = TRUE)
cat("\nConfusion Matrix:\n", file = log_file, append = TRUE)
print(confusion_matrix_2025$table, file = log_file, append = TRUE)

# ==================================================================================== #
#                             FEATURE IMPORTANCE ANALYSIS
# ==================================================================================== #

# Get feature importance
importance_matrix <- xgb.importance(feature_names = features, model = xgb_model)

# Create feature importance plot
importance_plot <- ggplot(importance_matrix, aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(
        title = "Feature Importance in XGBoost Model",
        x = "Features",
        y = "Gain (Contribution to Model)"
    )

# Calculate SHAP values for feature impact
shap_values <- shap.values(
    xgb_model = xgb_model, 
    X_train = as.matrix(clean_data[, features])
)

# Create SHAP summary plot
shap_data <- data.frame(
    Feature = rep(features, each = nrow(clean_data)),
    SHAP_value = as.vector(shap_values$shap_score),
    Feature_value = as.vector(as.matrix(clean_data[, features]))
)

shap_summary_plot <- ggplot(shap_data, aes(x = reorder(Feature, abs(SHAP_value), FUN = mean), y = SHAP_value)) +
    geom_violin(fill = "lightblue", alpha = 0.5) +
    coord_flip() +
    theme_minimal() +
    labs(
        title = "SHAP Value Distribution by Feature",
        x = "Features",
        y = "SHAP Value"
    )

# Save plots
saveRDS(roc_plot, "data/model_roc_plot.rds")
saveRDS(roc_plot_2025, "data/model_roc_plot_2025.rds")
saveRDS(importance_plot, "data/model_importance_plot.rds")
saveRDS(shap_summary_plot, "data/model_shap_plot.rds")

# Create summary table of all metrics
performance_summary <- data.frame(
    Metric = c("Historical Accuracy", "Historical AUC", "2025 Accuracy", "2025 AUC"),
    Value = c(
        confusion_matrix$overall["Accuracy"],
        auc_score,
        confusion_matrix_2025$overall["Accuracy"],
        auc_score_2025
    )
)

# Save summary metrics
saveRDS(performance_summary, "data/model_performance_summary.rds")
saveRDS(importance_matrix, "data/model_feature_importance.rds")
saveRDS(shap_values, "data/model_shap_values.rds")

# Create probability calibration plot
calibration_plot <- ggplot() +
    stat_bin(
        data = historical_predictions,
        aes(x = predicted_prob, y = ..density.., weight = actual),
        bins = 50
    ) +
    geom_line(
        data = data.frame(x = seq(0, 1, 0.01)),
        aes(x = x, y = x),
        linetype = "dashed",
        color = "red"
    ) +
    theme_minimal() +
    labs(
        title = "Probability Calibration Plot",
        x = "Predicted Probability",
        y = "Observed Frequency"
    )

saveRDS(calibration_plot, "data/model_calibration_plot.rds")

cat("\n✅ Model analysis completed successfully\n", file = log_file, append = TRUE)
