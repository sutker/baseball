# ==================================================================================== #
#                             EXPORT VISUALIZATIONS AND DATA
# ==================================================================================== #

# Clear environment
rm(list = ls())

# ===== Libraries =====
library(ggplot2)
library(readr)

# ===== Setup Logging =====
log_file <- "logs/script_09_output.log"
cat(format(Sys.time()), "Starting Visualization Exports\n", file = log_file)

# Function to export plot
export_plot <- function(plot_obj, filename, width = 10, height = 6, dpi = 300) {
    ggsave(
        filename = file.path("static/images", paste0(filename, ".png")),
        plot = plot_obj,
        width = width,
        height = height,
        dpi = dpi
    )
    cat(sprintf("✅ Exported %s\n", filename), file = log_file, append = TRUE)
}

# Load and export visualizations
# ROC Plots
roc_plot <- readRDS("data/model_roc_plot.rds")
roc_plot_2025 <- readRDS("data/model_roc_plot_2025.rds")
export_plot(roc_plot, "roc_plot_historical")
export_plot(roc_plot_2025, "roc_plot_2025")

# Feature Importance Plot
importance_plot <- readRDS("data/model_importance_plot.rds")
export_plot(importance_plot, "feature_importance")

# SHAP Plot
shap_plot <- readRDS("data/model_shap_plot.rds")
export_plot(shap_plot, "shap_summary")

# Model Calibration Plot
calibration_plot <- readRDS("data/model_calibration_plot.rds")
export_plot(calibration_plot, "probability_calibration")

# XGBoost Grid Search Plot
grid_search_plot <- readRDS("data/xgboost_grid_search_plot.rds")
export_plot(grid_search_plot, "grid_search_results")

# XGBoost Feature Importance Plot
xgboost_importance <- readRDS("data/xgboost_importance_plot.rds")
export_plot(xgboost_importance, "xgboost_feature_importance")

cat("\n✅ All visualizations exported successfully\n", file = log_file, append = TRUE)

# Print summary of exports
cat("\nExport Summary:\n", file = log_file, append = TRUE)
cat("Plots exported to static/images/:\n", file = log_file, append = TRUE)
cat("- roc_plot_historical.png\n", file = log_file, append = TRUE)
cat("- roc_plot_2025.png\n", file = log_file, append = TRUE)
cat("- feature_importance.png\n", file = log_file, append = TRUE)
cat("- shap_summary.png\n", file = log_file, append = TRUE)
cat("- probability_calibration.png\n", file = log_file, append = TRUE)
cat("- grid_search_results.png\n", file = log_file, append = TRUE)
cat("- xgboost_feature_importance.png\n", file = log_file, append = TRUE)
