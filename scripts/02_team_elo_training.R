# ==================================================================================== #
#                                 TEAM ELO PARAMETER OPTIMIZATION
# ==================================================================================== #

# Clear environment
rm(list = ls())

# ===== Libraries =====
library(dplyr)
library(ggplot2)
library(readr)

# ===== Load Data =====
# Load MasterGameInformation from script 1
MasterGameInformation <- readRDS("data/MasterGameInformation.rds")

# Load TeamList from script 1
teams <- teams_lu_table %>%
  filter(sport.name == "Major League Baseball")

TeamList <- teams %>%
  select(id, name, bref_abbreviation, abbreviation, venue.id, venue.name, league.id, league.name, division.id, division.name)

# ==================================================================================== #
#             GRID SEARCH: Optimize K, Max Cap, Decay Rate, Floor using RMSE
# ==================================================================================== #

# Define parameter ranges
K_vals <- c(20, 40, 60, 80)
cap_vals <- c(20, 30, 40, 50)
decay_rates <- c(0.7, 0.8, 0.9)
floor_vals <- c(5, 10, 15, 20)

results <- data.frame()
total_iterations <- length(K_vals) * length(cap_vals) * length(decay_rates) * length(floor_vals)
iteration <- 0

# Log file setup
log_file <- "logs/script_02_output.log"
cat(format(Sys.time()), "Starting grid search\n", file = log_file)

# Loop through all combinations
for (K in K_vals) {
  for (cap in cap_vals) {
    for (decay in decay_rates) {
      for (floor_val in floor_vals) {
        iteration <- iteration + 1
        message <- sprintf("Processing %d of %d: K=%d, cap=%d, decay=%.2f, floor=%d\n", 
                         iteration, total_iterations, K, cap, decay, floor_val)
        cat(message)
        cat(format(Sys.time()), message, file = log_file, append = TRUE)
        
        # Initialize Elo ratings
        elo_table <- TeamList %>% 
          select(team_id = id) %>% 
          mutate(current_elo = 1000)
        
        elo_log <- list()
        previous_season <- NA
        
        # Simulate through all games
        for (i in seq_len(nrow(MasterGameInformation))) {
          row <- MasterGameInformation[i, ]
          if (is.na(row$teams.home.score) || is.na(row$teams.away.score)) next
          
          current_season <- row$season
          if (!is.na(previous_season) && current_season != previous_season) {
            elo_table$current_elo <- 1000  # Reset at season start
          }
          previous_season <- current_season
          
          home <- row$teams.home.team.id
          away <- row$teams.away.team.id
          
          elo_h <- elo_table$current_elo[elo_table$team_id == home]
          elo_a <- elo_table$current_elo[elo_table$team_id == away]
          
          expected <- 1 / (1 + 10 ^ ((elo_a - elo_h) / 400))
          outcome <- ifelse(row$teams.home.score > row$teams.away.score, 1, 0)
          
          margin <- abs(row$teams.home.score - row$teams.away.score)
          margin_multiplier <- log(margin + 1)
          
          season_progress <- i / nrow(MasterGameInformation)
          dyn_cap <- max(cap * (1 - decay * season_progress), floor_val)
          
          delta <- K * margin_multiplier * (outcome - expected)
          delta <- max(min(delta, dyn_cap), -dyn_cap)
          
          elo_table$current_elo[elo_table$team_id == home] <- elo_h + delta
          elo_table$current_elo[elo_table$team_id == away] <- elo_a - delta
          
          elo_log[[i]] <- data.frame(
            team_id = c(home, away),
            elo = c(elo_h + delta, elo_a - delta),
            opponent_elo = c(elo_a, elo_h),
            win = c(outcome, 1-outcome)
          )
        }
        
        # Combine logs and calculate metrics
        elo_df <- bind_rows(elo_log)
        
        final_summary <- elo_df %>%
          group_by(team_id) %>%
          summarise(
            games = n(),
            wins = sum(win),
            final_elo = last(elo),
            win_pct = wins/games,
            .groups = "drop"
          )
        
        # Calculate opponent Elo correlation
        opp_strength <- elo_df %>%
          group_by(team_id) %>%
          summarise(
            avg_opp_elo = mean(opponent_elo),
            .groups = "drop"
          )
        
        final_summary <- final_summary %>% 
          left_join(opp_strength, by = "team_id")
        
        # Fit model and calculate metrics
        model <- lm(final_elo ~ win_pct, data = final_summary)
        rmse <- sqrt(mean((final_summary$final_elo - predict(model))^2))
        r2 <- summary(model)$r.squared
        opp_corr <- cor(final_summary$final_elo, final_summary$avg_opp_elo)
        
        # Store results
        results <- bind_rows(results, 
                           data.frame(K, cap, decay, floor_val, rmse, r2, opp_corr))
        
        # Log results
        log_message <- sprintf("Results - RMSE: %.3f, R²: %.3f, Opp Corr: %.3f\n",
                             rmse, r2, opp_corr)
        cat(format(Sys.time()), log_message, file = log_file, append = TRUE)
      }
    }
  }
}

# Calculate composite score
results_scaled <- results %>%
  mutate(
    rmse_scaled = (rmse - min(rmse)) / (max(rmse) - min(rmse)),
    opp_scaled = (abs(opp_corr) - min(abs(opp_corr))) / 
                 (max(abs(opp_corr)) - min(abs(opp_corr)))
  ) %>%
  mutate(
    composite_score = 0.6 * (1 - rmse_scaled) + 0.4 * opp_scaled
  ) %>%
  arrange(desc(composite_score))

# Create performance plot
TeamEloParameterPlot <- ggplot(results, aes(x = r2, y = rmse, label = K)) +
  geom_point(aes(color = as.factor(K)), size = 3) +
  geom_text(vjust = 1.5, size = 3) +
  labs(
    title = "RMSE vs R²: Grid Search Results",
    x = "R-squared (final_elo ~ win_pct)",
    y = "RMSE",
    color = "K value"
  ) +
  theme_minimal()

# Save outputs
saveRDS(results_scaled, "data/results_scaled.rds")
saveRDS(TeamEloParameterPlot, "data/TeamEloParameterPlot.rds")

# Log completion
cat(format(Sys.time()), "Grid search completed. Results saved.\n", 
    file = log_file, append = TRUE)

# Print best parameters
best_params <- results_scaled %>% slice(1)
cat("\nBest parameters found:\n")
cat("K:", best_params$K, "\n")
cat("Cap:", best_params$cap, "\n")
cat("Decay:", best_params$decay, "\n")
cat("Floor:", best_params$floor_val, "\n")
cat("Composite Score:", round(best_params$composite_score, 4), "\n")
