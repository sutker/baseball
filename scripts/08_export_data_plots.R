# ==================================================================================== #
#                             DATA AND PLOT EXPORTS
# ==================================================================================== #

# Clear environment
rm(list = ls())

# ===== Libraries =====
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(gganimate)
library(ggrepel)

# ===== Setup Logging =====
log_file <- "logs/script_08_output.log"
cat(format(Sys.time()), "Starting Data and Plot Exports\n", file = log_file)

# ===== Load All Required Data =====
cat("Loading data from previous scripts...\n", file = log_file, append = TRUE)

# Load data from scripts 1-5
datasets <- list(
  # Team Summaries
  "TeamFinalSummary" = "data/TeamFinalSummary.rds",
  "TeamFinalSummary2025" = "data/TeamFinalSummary2025.rds",
  "PlayerFinalEloWithNames2025" = "data/PlayerFinalEloWithNames2025.rds",
  
  # Game-level Data
  "TrainingDataClean" = "data/TrainingDataClean.rds",
  "TrainingDataClean2025" = "data/TrainingDataClean2025.rds",
  "TeamEloLog" = "data/TeamEloLog.rds",
  "TeamEloLog2025" = "data/TeamEloLog2025.rds",
  "PlayerEloLog2025" = "data/PlayerEloLog2025.rds",
  
  # Parameters and Results
  "results_scaled" = "data/results_scaled.rds",
  "TeamEloParameterPlot" = "data/TeamEloParameterPlot.rds",

  # Team Statistics 2025
  "TeamPitchingStatistics2025" = "data/TeamPitchingStatistics2025.rds",
  "TeamBattingStatistics2025" = "data/TeamBattingStatistics2025.rds",
  "TeamFieldingStatistics2025" = "data/TeamFieldingStatistics2025.rds",
  "TeamBattingStatistics2024" = "data/TeamBattingStatistics2024.rds",
  "TeamPitchingStatistics2024" = "data/TeamPitchingStatistics2024.rds",

  # Prediction Coefficients
  "glm_home_coefficients" = "data/glm_home_coefficients.rds",
  "glm_away_coefficients" = "data/glm_away_coefficients.rds",

  # Miscellaneous
  "Rosters2025" = "data/Rosters2025.rds",
  "GameInformation2025" = "data/GameInformation2025.rds",
  "MasterGameInformation" = "data/MasterGameInformation.rds"
)

# Load all datasets
for (name in names(datasets)) {
  tryCatch({
    assign(name, readRDS(datasets[[name]]))
    cat(sprintf("✅ Loaded %s\n", name), file = log_file, append = TRUE)
  }, error = function(e) {
    cat(sprintf("❌ Failed to load %s: %s\n", name, conditionMessage(e)), 
        file = log_file, append = TRUE)
  })
}

# ==================================================================================== #
#                             CSV EXPORTS
# ==================================================================================== #

# Define safe export function
safe_export <- function(data, filename, base_path = "data") {
    tryCatch({
        write_csv(data, file.path(base_path, paste0(filename, ".csv")))
        cat(sprintf("✅ Exported %s.csv\n", filename), file = log_file, append = TRUE)
    }, error = function(e) {
        cat(sprintf("❌ Failed to export %s: %s\n", filename, conditionMessage(e)), 
            file = log_file, append = TRUE)
    })
}

# Export Final Summaries
cat("\nExporting final summaries...\n", file = log_file, append = TRUE)
safe_export(TeamFinalSummary, "TeamFinalSummary")
safe_export(TeamFinalSummary2025, "TeamFinalSummary2025")
safe_export(PlayerFinalEloWithNames2025, "PlayerFinalEloWithNames2025")

# Export Training Data
cat("\nExporting training data...\n", file = log_file, append = TRUE)
safe_export(TrainingDataClean, "TrainingDataClean")
safe_export(TrainingDataClean2025, "TrainingDataClean2025")

# Export Game Logs
cat("\nExporting game logs...\n", file = log_file, append = TRUE)
safe_export(TeamEloLog, "TeamEloLog")
safe_export(TeamEloLog2025, "TeamEloLog2025")
safe_export(PlayerEloLog2025, "PlayerEloLog2025")

# Export Team Statistics 2025
cat("\nExporting team statistics 2025...\n", file = log_file, append = TRUE)
safe_export(TeamPitchingStatistics2025, "TeamPitchingStatistics2025")
safe_export(TeamBattingStatistics2025, "TeamBattingStatistics2025")
safe_export(TeamFieldingStatistics2025, "TeamFieldingStatistics2025")
safe_export(TeamBattingStatistics2024, "TeamBattingStatistics2024")
safe_export(TeamPitchingStatistics2024, "TeamPitchingStatistics2024")

# Export Miscellaneous Data
cat("\nExporting miscellaneous data...\n", file = log_file, append = TRUE)
safe_export(Rosters2025, "Rosters2025")
safe_export(GameInformation2025, "GameInformation2025")
safe_export(MasterGameInformation, "MasterGameInformation")
safe_export(results_scaled, "results_scaled")
safe_export(glm_home_coefficients, "glm_home_coefficients")
safe_export(glm_away_coefficients, "glm_away_coefficients")

# ==================================================================================== #
#                             PLOT EXPORTS
# ==================================================================================== #

# Define plot export function
export_plot <- function(plot_obj, filename, width = 10, height = 6, dpi = 300, 
                       base_path = "static/images") {
    tryCatch({
        ggsave(
            filename = file.path(base_path, paste0(filename, ".png")),
            plot = plot_obj,
            width = width,
            height = height,
            dpi = dpi
        )
        cat(sprintf("✅ Exported %s.png\n", filename), file = log_file, append = TRUE)
    }, error = function(e) {
        cat(sprintf("❌ Failed to export %s: %s\n", filename, conditionMessage(e)), 
            file = log_file, append = TRUE)
    })
}

# === 1. Grid Search Performance Plot ===
cat("\nExporting Grid Search performance plot...\n", file = log_file, append = TRUE)
export_plot(TeamEloParameterPlot, "TeamElo_GridSearch_Performance")

# === 2. Final Elo vs Win Percentage (2021-2024) ===
cat("\nCreating and exporting historical Elo vs Win % plot...\n", file = log_file, append = TRUE)
plot_elo_legacy <- ggplot(TeamFinalSummary, aes(x = win_pct, y = final_elo)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
    geom_text_repel(aes(label = team_name), size = 3) +
    labs(
        title = "Final Elo vs Win Percentage (2021-2024)",
        x = "Win Percentage",
        y = "Final Elo"
    ) +
    theme_minimal(base_size = 14)

export_plot(plot_elo_legacy, "FinalElo_vs_WinPct_2021_2024")

# === 3. Final Elo vs Win Percentage (2025) ===
cat("\nCreating and exporting 2025 Elo vs Win % plot...\n", file = log_file, append = TRUE)
plot_elo_2025 <- ggplot(TeamFinalSummary2025, aes(x = win_pct, y = final_elo)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
    geom_text_repel(aes(label = team_name), size = 3) +
    labs(
        title = "Final Elo vs Win Percentage (2025)",
        x = "Win Percentage",
        y = "Final Elo"
    ) +
    theme_minimal(base_size = 14)

export_plot(plot_elo_2025, "FinalElo_vs_WinPct_2025")

# === 4. Team Elo Progression (Yankees Example) ===
cat("\nCreating and exporting team Elo progression plots...\n", file = log_file, append = TRUE)

team_plot_id <- 112  # Yankees
team_name <- TeamFinalSummary2025 %>%
    filter(team_id == team_plot_id) %>%
    pull(team_name)

team_elo_data <- TeamEloLog2025 %>%
    filter(home_team_id == team_plot_id | away_team_id == team_plot_id) %>%
    mutate(
        team_elo = ifelse(home_team_id == team_plot_id, home_elo_after, away_elo_after),
        opponent_id = ifelse(home_team_id == team_plot_id, away_team_id, home_team_id),
        team_score = ifelse(home_team_id == team_plot_id, home_score, away_score),
        opponent_score = ifelse(home_team_id == team_plot_id, away_score, home_score),
        score_label = paste0(team_score, "-", opponent_score),
        outcome = case_when(
            team_score > opponent_score ~ "Win",
            team_score < opponent_score ~ "Loss",
            TRUE ~ "Tie"
        ),
        win = ifelse(outcome == "Win", 1, 0),
        loss = ifelse(outcome == "Loss", 1, 0)
    ) %>%
    arrange(as.Date(date))

# Static plot
plot_elo_progression <- ggplot(team_elo_data, aes(x = as.Date(date), y = team_elo)) +
    geom_line(size = 1.1, color = "#1f1f1f") +
    geom_point(aes(color = outcome), size = 3) +
    scale_color_manual(values = c("Win" = "#1b9e77", "Loss" = "#d95f02", "Tie" = "gray50")) +
    geom_text_repel(
        aes(label = score_label),
        size = 3,
        box.padding = 0.5,
        point.padding = 0.2
    ) +
    labs(
        title = paste("Elo Rating Progression:", team_name),
        x = "Date",
        y = "Elo Rating",
        color = "Outcome"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")

export_plot(plot_elo_progression, "team_elo_progression")

# Animated plot
plot_elo_animated <- plot_elo_progression +
    transition_reveal(as.Date(date)) +
    labs(title = paste("Elo Rating Progression:", team_name, "\n{frame_along}"))

# Save animation
anim_save(
    "team_elo_progression_animated.gif",
    animation = animate(
        plot_elo_animated,
        width = 800,
        height = 500,
        fps = 10,
        duration = 15,
        renderer = gifski_renderer()
    ),
    path = "static/images"
)

cat("\n✅ All exports completed successfully\n", file = log_file, append = TRUE)

# Print summary of exports
cat("\nExport Summary:\n", file = log_file, append = TRUE)
cat("CSV files exported:", length(datasets) - 1, "\n", file = log_file, append = TRUE)
cat("Static plots exported: 4\n", file = log_file, append = TRUE)
cat("Animated plots exported: 1\n", file = log_file, append = TRUE)
