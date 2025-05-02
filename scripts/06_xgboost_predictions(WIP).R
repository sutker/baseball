# ==================================================================================== #
#                             XGBOOST GAME OUTCOME PREDICTIONS
# ==================================================================================== #

# Clear environment
rm(list = ls())

# ===== Libraries =====
library(dplyr)
library(readr)
library(xgboost)
library(caret)
library(Matrix)
library(ggplot2)
library(SHAPforxgboost)
library(car)      # Added for VIF analysis
library(corrplot) # Added for correlation plot
library(pROC)     # Added for ROC analysis

# Create log directory if it doesn't exist
dir.create("logs", showWarnings = FALSE)

# ===== Setup Logging =====
log_file <- "logs/script_06_output.log"
sink(log_file)  # Redirect all output to log file
cat(format(Sys.time()), "Starting XGBoost Game Outcome Predictions\n\n")

# ===== Load Data =====
# Load game information
MasterGameInformation <- readRDS("data/MasterGameInformation.rds")
GameInformation2025 <- readRDS("data/GameInformation2025.rds")

# Load Elo logs
TeamEloLog <- readRDS("data/TeamEloLog.rds")
TeamEloLog2025 <- readRDS("data/TeamEloLog2025.rds")

# Team name abbreviation mapping for stats
fielding_map <- c(
    "Diamondbacks" = "ARI", "Braves" = "ATL", "Orioles" = "BAL", "Red Sox" = "BOS",
    "Cubs" = "CHC", "White Sox" = "CHW", "Reds" = "CIN", "Guardians" = "CLE",
    "Rockies" = "COL", "Tigers" = "DET", "Astros" = "HOU", "Royals" = "KCR",
    "Angels" = "LAA", "Dodgers" = "LAD", "Marlins" = "MIA", "Brewers" = "MIL",
    "Twins" = "MIN", "Mets" = "NYM", "Yankees" = "NYY", "Athletics" = "OAK",
    "Phillies" = "PHI", "Pirates" = "PIT", "Padres" = "SDP", "Giants" = "SFG",
    "Mariners" = "SEA", "Cardinals" = "STL", "Rays" = "TBR", "Rangers" = "TEX",
    "Blue Jays" = "TOR", "Nationals" = "WSN"
)

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

# ===== Helper function to merge stats and calculate differences =====
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

# Prepare features and target variables
features <- c("OBP_diff", "SLG_diff", "ISO_diff", "wOBA_diff", "wRC_diff", "WAR_diff",
             "ERA_diff", "FIP_diff", "Kpct_diff", "BBpct_diff", "Elo_diff", "HomeFieldAdvantage")

# Create target (1 if home team wins, 0 if away team wins)
GameData$target <- ifelse(GameData$teams.home.score > GameData$teams.away.score, 1, 0)
GameData2025$target <- ifelse(GameData2025$teams.home.score > GameData2025$teams.away.score, 1, 0)

# Clean data for analysis
clean_data <- GameData %>% select(all_of(c(features, "target"))) %>% na.omit()

# Check for zero variance predictors
feature_summary <- sapply(clean_data[features], function(x) {
    c(sd = sd(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE),
      n_unique = length(unique(x)))
})

# Log feature summary
cat("\nFeature Summary:\n", file = log_file, append = TRUE)
capture.output(print(feature_summary), file = log_file, append = TRUE)

# Remove zero variance predictors
non_zero_var_features <- features[feature_summary["sd",] > 0]
cat("\nRemoving zero variance predictors...\n", file = log_file, append = TRUE)
cat("Features removed:", 
    paste(setdiff(features, non_zero_var_features), collapse = ", "), 
    "\n", file = log_file, append = TRUE)

# Update feature data for correlation analysis
feature_data <- clean_data[non_zero_var_features]

# Create correlation matrix with error handling
tryCatch({
    cor_matrix <- cor(feature_data, use = "pairwise.complete.obs")
    
    # Create correlation plot
    png("data/correlation_matrix.png", width = 800, height = 800)
    corrplot(cor_matrix, 
             method = "color", 
             type = "upper", 
             order = "hclust",
             addCoef.col = "black",
             number.cex = 0.7,
             tl.cex = 0.7,
             title = "Correlation Matrix of Non-Zero Variance Predictors")
    dev.off()
    
    # Identify highly correlated pairs (|r| > 0.7)
    high_cor <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
    if(nrow(high_cor) > 0) {
        cor_pairs <- data.frame(
            var1 = rownames(cor_matrix)[high_cor[,1]],
            var2 = colnames(cor_matrix)[high_cor[,2]],
            correlation = cor_matrix[high_cor]
        )
        cat("\nHighly correlated feature pairs (|r| > 0.7):\n", file = log_file, append = TRUE)
        capture.output(print(cor_pairs), file = log_file, append = TRUE)
    }
    
    # Calculate VIF scores for remaining features
    model_for_vif <- lm(target ~ ., data = clean_data[c(non_zero_var_features, "target")])
    vif_scores <- vif(model_for_vif)
    
    # Log VIF scores
    cat("\nVariance Inflation Factors:\n", file = log_file, append = TRUE)
    capture.output(print(vif_scores), file = log_file, append = TRUE)
    
    # Select features based on VIF and correlation
    high_vif_features <- names(vif_scores)[vif_scores > 5]
    if(length(high_vif_features) > 0) {
        cat("\nFeatures with high VIF (> 5):\n", file = log_file, append = TRUE)
        capture.output(print(high_vif_features), file = log_file, append = TRUE)
    }
    
    # Update final feature set
    optimal_features <- setdiff(non_zero_var_features, high_vif_features)
    
    cat("\nFinal selected features:\n", file = log_file, append = TRUE)
    capture.output(print(optimal_features), file = log_file, append = TRUE)
    
}, error = function(e) {
    cat("\nError in correlation analysis:", conditionMessage(e), "\n", file = log_file, append = TRUE)
})

# Update features for model training
features <- optimal_features

# Split data into training, validation, and test sets
set.seed(42)  # for reproducibility
train_idx <- sample(1:nrow(clean_data), 0.7 * nrow(clean_data))
remaining_idx <- setdiff(1:nrow(clean_data), train_idx)
valid_idx <- sample(remaining_idx, 0.5 * length(remaining_idx))
test_idx <- setdiff(remaining_idx, valid_idx)

train_data <- clean_data[train_idx, ]
valid_data <- clean_data[valid_idx, ]
test_data <- clean_data[test_idx, ]

# Log data splitting information
cat("\nData Split Summary:\n", file = log_file, append = TRUE)
cat(sprintf("Training set size: %d samples\n", nrow(train_data)), file = log_file, append = TRUE)
cat(sprintf("Validation set size: %d samples\n", nrow(valid_data)), file = log_file, append = TRUE)
cat(sprintf("Test set size: %d samples\n", nrow(test_data)), file = log_file, append = TRUE)

# Convert to xgb.DMatrix
dtrain <- xgb.DMatrix(data = as.matrix(train_data[, features]), label = train_data$target)
dtest <- xgb.DMatrix(data = as.matrix(test_data[, features]), label = test_data$target)
dvalid <- xgb.DMatrix(data = as.matrix(valid_data[, features]), label = valid_data$target)

# Grid search parameters
param_grid <- expand.grid(
    eta = c(0.01, 0.1, 0.3),
    max_depth = c(3, 5, 7),
    min_child_weight = c(1, 3, 5),
    subsample = c(0.8, 0.9, 1.0),
    colsample_bytree = c(0.8, 0.9, 1.0)
)

# Initialize results storage
grid_results <- data.frame()

# Grid search
total_combinations <- nrow(param_grid)
cat("\nStarting grid search...\n", file = log_file, append = TRUE)
cat(sprintf("Total parameter combinations to test: %d\n", total_combinations), file = log_file, append = TRUE)
cat("Parameter combinations being tested:\n", file = log_file, append = TRUE)
print(param_grid, file = log_file, append = TRUE)
cat("\nProgress:\n", file = log_file, append = TRUE)

for (i in 1:nrow(param_grid)) {
    # Log current progress
    progress <- sprintf("[%d/%d] Testing parameters: eta=%.2f, max_depth=%d, min_child_weight=%d, subsample=%.1f, colsample_bytree=%.1f",
                       i, total_combinations,
                       param_grid$eta[i],
                       param_grid$max_depth[i],
                       param_grid$min_child_weight[i],
                       param_grid$subsample[i],
                       param_grid$colsample_bytree[i])
    cat(progress, "\n", file = log_file, append = TRUE)
    
    params <- list(
        objective = "binary:logistic",
        eval_metric = "logloss",
        eta = param_grid$eta[i],
        max_depth = param_grid$max_depth[i],
        min_child_weight = param_grid$min_child_weight[i],
        subsample = param_grid$subsample[i],
        colsample_bytree = param_grid$colsample_bytree[i]
    )
    
    cv_model <- xgb.cv(
        params = params,
        data = dtrain,
        nrounds = 1000,
        nfold = 5,
        early_stopping_rounds = 50,
        verbose = 0
    )
    
    grid_results <- rbind(grid_results, 
                         data.frame(param_grid[i, ],
                                  best_iteration = cv_model$best_iteration,
                                  best_logloss = min(cv_model$evaluation_log$test_logloss_mean)))
}

# Find best parameters
best_params <- grid_results[which.min(grid_results$best_logloss), ]

# Train final model with best parameters
final_params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = best_params$eta,
    max_depth = best_params$max_depth,
    min_child_weight = best_params$min_child_weight,
    subsample = best_params$subsample,
    colsample_bytree = best_params$colsample_bytree
)

final_model <- xgb.train(
    params = final_params,
    data = dtrain,
    nrounds = best_params$best_iteration,
    watchlist = list(train = dtrain, test = dtest),
    verbose = 0
)

# Get feature importance
importance_matrix <- xgb.importance(feature_names = features, model = final_model)

# Reintroduce some removed variables for further analysis
reintroduced_features <- c("OBP_diff", "SLG_diff", "ISO_diff", "wOBA_diff", "wRC_diff", "WAR_diff", "ERA_diff", "FIP_diff")
all_features <- c(features, reintroduced_features)

# Update feature data for reintroduced variables
feature_data <- clean_data[all_features]

# Log reintroduced features
cat("\nReintroduced Features for Further Analysis:\n", file = log_file, append = TRUE)
cat(paste(reintroduced_features, collapse = ", "), "\n", file = log_file, append = TRUE)

# Proceed with SHAP and feature importance analysis for all features

# SHAP Analysis for all features
cat("\nPerforming SHAP Analysis for All Features...\n", file = log_file, append = TRUE)

# Calculate SHAP values for all features
shap_long_all <- shap.prep(xgb_model = final_model, X_train = as.matrix(train_data[, all_features]))
shap_viz_all <- shap.plot.summary(shap_long_all, scientific = FALSE)

# Save SHAP values and plot for all features
saveRDS(shap_long_all, "data/model_shap_values_all.rds")
saveRDS(shap_viz_all, "data/model_shap_plot_all.rds")

# Log SHAP analysis summary for all features
cat("\nSHAP Analysis Summary for All Features:\n", file = log_file, append = TRUE)
shap_summary_all <- sapply(all_features, function(feat) {
    mean(abs(shap_long_all$shap_value[shap_long_all$variable == feat]))
})
print(sort(shap_summary_all, decreasing = TRUE), file = log_file, append = TRUE)

cat("\nFeature Analysis Complete!\n", file = log_file, append = TRUE)

# Save model and predictions
saveRDS(final_model, "data/xgboost_final_model.rds")
saveRDS(TeamEloLog2025, "data/TeamEloLog2025.rds")

# Calculate and log model performance metrics
test_predictions <- predict(final_model, as.matrix(test_data[, features]))
test_labels <- test_data$target

# Calculate metrics
accuracy <- mean(round(test_predictions) == test_labels)
auc_score <- auc(roc(test_labels, test_predictions))
precision <- posPredValue(factor(round(test_predictions)), factor(test_labels))
recall <- sensitivity(factor(round(test_predictions)), factor(test_labels))
f1_score <- 2 * (precision * recall) / (precision + recall)

# Log performance metrics
cat("\nModel Performance Metrics:\n", file = log_file, append = TRUE)
cat(sprintf("Accuracy: %.4f\n", accuracy), file = log_file, append = TRUE)
cat(sprintf("AUC-ROC: %.4f\n", auc_score), file = log_file, append = TRUE)
cat(sprintf("Precision: %.4f\n", precision), file = log_file, append = TRUE)
cat(sprintf("Recall: %.4f\n", recall), file = log_file, append = TRUE)
cat(sprintf("F1 Score: %.4f\n", f1_score), file = log_file, append = TRUE)

# Log feature importance details
cat("\nFeature Importance Summary:\n", file = log_file, append = TRUE)
print(importance_matrix, file = log_file, append = TRUE)

# Save performance metrics
performance_summary <- list(
    accuracy = accuracy,
    auc = auc_score,
    precision = precision,
    recall = recall,
    f1_score = f1_score
)
saveRDS(performance_summary, "data/model_performance_summary.rds")

# Feature Importance Analysis for Reintroduced Variables
cat("\nAnalyzing Feature Importance for Reintroduced Variables...\n", file = log_file, append = TRUE)

# Train model with all features
final_model_all <- xgb.train(
    params = final_params,
    data = xgb.DMatrix(data = as.matrix(train_data[, all_features]), label = train_data$target),
    nrounds = best_params$best_iteration,
    watchlist = list(train = dtrain, test = dtest),
    verbose = 0
)

# Get feature importance for all features
importance_matrix_all <- xgb.importance(feature_names = all_features, model = final_model_all)

# Log feature importance for all features
cat("\nFeature Importance for All Features:\n", file = log_file, append = TRUE)
print(importance_matrix_all, file = log_file, append = TRUE)

# Save feature importance plot
xgb.plot.importance(importance_matrix_all, top_n = 20, main = "Feature Importance for All Features")
ggsave("data/xgboost_importance_plot_all.png")

# End of script
cat("\nScript completed at:", format(Sys.time()), "\n", file = log_file, append = TRUE)
