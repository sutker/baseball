rm(list = ls())

library(baseballr)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(tibble)
library(ggplot2)

teams <- teams_lu_table %>% filter(sport.name == "Major League Baseball")

TeamList <- teams %>%
  select(id, name, bref_abbreviation, abbreviation, venue.id, venue.name, league.id, league.name, division.id, division.name)

team_abbr_lookup <- tibble::tibble(
  team_name = c(
    "Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles", "Boston Red Sox", "Chicago Cubs",
    "Chicago White Sox", "Cincinnati Reds", "Cleveland Indians", "Colorado Rockies", "Detroit Tigers",
    "Houston Astros", "Kansas City Royals", "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins",
    "Milwaukee Brewers", "Minnesota Twins", "New York Mets", "New York Yankees", "Oakland Athletics",
    "Philadelphia Phillies", "Pittsburgh Pirates", "San Diego Padres", "San Francisco Giants",
    "Seattle Mariners", "St. Louis Cardinals", "Tampa Bay Rays", "Texas Rangers", "Toronto Blue Jays",
    "Washington Nationals"
  ),
  abbr = c(
    "ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", 
    "HOU", "KCR", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", 
    "PHI", "PIT", "SDP", "SFG", "SEA", "STL", "TBR", "TEX", "TOR", "WSN"
  )
)

short_names <- c(
  "ARI" = "Diamondbacks", "ATL" = "Braves", "BAL" = "Orioles", "BOS" = "Red Sox", 
  "CHC" = "Cubs", "CHW" = "White Sox", "CIN" = "Reds", "CLE" = "Indians", 
  "COL" = "Rockies", "DET" = "Tigers", "HOU" = "Astros", "KCR" = "Royals", 
  "LAA" = "Angels", "LAD" = "Dodgers", "MIA" = "Marlins", "MIL" = "Brewers", 
  "MIN" = "Twins", "NYM" = "Mets", "NYY" = "Yankees", "OAK" = "Athletics", 
  "PHI" = "Phillies", "PIT" = "Pirates", "SDP" = "Padres", "SFG" = "Giants", 
  "SEA" = "Mariners", "STL" = "Cardinals", "TBR" = "Rays", "TEX" = "Rangers", 
  "TOR" = "Blue Jays", "WSN" = "Nationals"
)

TeamList <- TeamList %>%
  left_join(team_abbr_lookup, by = c("name" = "team_name")) %>%
  mutate(short_name = short_names[abbr])

TeamEloLog2025 <- readRDS("data/TeamEloLog2025.rds")

create_team_plot <- function(team_id) {
  Team_plot_name <- TeamList %>% filter(id == team_id) %>% pull(name)
  Team_short_name <- TeamList %>% filter(id == team_id) %>% pull(short_name)
  
  Team_team_elo_2025 <- TeamEloLog2025 %>%
    filter(home_team_id == team_id | away_team_id == team_id) %>%
    mutate(
      Team_team_elo = ifelse(home_team_id == team_id, home_elo_after, away_elo_after),
      opponent_id = ifelse(home_team_id == team_id, away_team_id, home_team_id),
      team_score = ifelse(home_team_id == team_id, home_score, away_score),
      opponent_score = ifelse(home_team_id == team_id, away_score, home_score),
      score_label = paste0(team_score, "–", opponent_score),
      outcome = case_when(
        team_score > opponent_score ~ "Win",
        team_score < opponent_score ~ "Loss",
        TRUE ~ "Tie"
      ),
      season = lubridate::year(as.Date(date)),
      win = ifelse(outcome == "Win", 1, 0),
      loss = ifelse(outcome == "Loss", 1, 0)
    ) %>%
    group_by(season) %>%
    mutate(
      cum_win = cumsum(win),
      cum_loss = cumsum(loss),
      win_ratio = cum_win / (cum_win + cum_loss),
      win_ratio_scaled = 500 + win_ratio * 1000
    ) %>%
    ungroup() %>%
    left_join(TeamList %>% select(id, opponent_abbr = abbreviation), by = c("opponent_id" = "id")) %>%
    arrange(as.Date(date)) %>%
    mutate(frame = row_number())

  final_point <- Team_team_elo_2025 %>%
    arrange(as.Date(date)) %>%
    slice_tail(n = 1)

  plot_obj <- ggplot(Team_team_elo_2025, aes(x = as.Date(date))) +
    geom_line(aes(y = Team_team_elo), color = "#1f1f1f", size = 1.1) +
    geom_line(aes(y = win_ratio_scaled), color = "#1b9e77", linetype = "dashed", size = 1) +
    geom_text(data = final_point,
              aes(y = Team_team_elo, label = paste0("Elo: ", round(Team_team_elo))),
              hjust = 1.1, vjust = -5.1, color = "#1f1f1f", fontface = "bold") +
    geom_text(data = final_point,
              aes(y = win_ratio_scaled, label = paste0("Win Ratio: ", scales::percent(win_ratio))),
              hjust = 1.1, vjust = -8.1, color = "#1b9e77", fontface = "bold") +
    scale_y_continuous(
      name = "Elo Rating", limits = c(500, 1500),
      sec.axis = sec_axis(~ (. - 500) / 1000, name = "Win Ratio", labels = scales::percent_format())
    ) +
    scale_x_date(expand = expansion(mult = c(0.01, 0.05))) +
    labs(
      title = paste("📈 Elo & Win Ratio over Time for", Team_plot_name, "(2025)"),
      subtitle = "Solid = Elo | Dashed = Win Ratio",
      x = "Date", y = "Elo Rating"
    ) +
    theme_minimal(base_size = 14)
    
  export_plot(plot_obj, gsub(" ", "", paste0(tolower(Team_short_name), "_WinRatio_2025")))
}

plot_path <- "C:/Users/micha/OneDrive/Desktop/BaseballProject/static/images/TeamElo2025"

export_plot <- function(plot_obj, filename, width = 10, height = 6, dpi = 300) {
  ggsave(filename = file.path(plot_path, paste0(filename, ".png")),
         plot = plot_obj, width = width, height = height, dpi = dpi)
  cat("✅ Saved:", filename, "\n")
}

# Create directory if it doesn't exist
if (!dir.exists(plot_path)) {
  dir.create(plot_path, recursive = TRUE)
}

# Loop through all team IDs and create plots
for (team_id in TeamList$id) {
  cat("Creating plot for", TeamList$name[TeamList$id == team_id], "\n")
  create_team_plot(team_id)
}
