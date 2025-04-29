# Web Scraping and Wrangling Data
# Individual Player Statistics, NBA

# Paul Grajzl

# This file draws on Basketball Reference data to synthesize season-specific and time-period specific data based on either player seasons or players in general
# This script contains a mix of scraping and wrangling code

# load packages
library(tidyverse)
library(rvest)
library(robotstxt)
library(dplyr)
library(stringr)
library(tidyverse)

# Scraping check (only done once)
paths_allowed("https://www.basketball-reference.com/leagues/NBA_2024_totals.html")

### INITIAL DATA SCRAPING ###

# Define the range of years
start_year <- 2024
end_year <- 2015

# Loop over the years and store each table with a unique name
for (year in start_year:end_year) {
  # Construct the season name (e.g., "23_24" for 2024)
  season_start <- year - 1
  season_end <- substr(year, 3, 4)
  season_label <- paste0(season_start %% 100, "_", season_end)
  
  # Build the URL for each season
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_totals.html")
  
  # Scrape the tables
  tables <- url |>
    read_html() |>
    html_elements("table")
  
  # Extract the first table (player totals) and assign to a named variable, just so happened to be the first table in the list this time around
  assign(
    paste0("Player_stats", season_label),
    tables |>
      purrr::pluck(1) |>
      html_table()
  )
}

# Save each dataset to a CSV file - this will give us the individual season datasets for all ten seasons
for (year in start_year:end_year) {
  # Construct season label (same logic as before)
  season_start <- year - 1
  season_end <- substr(year, 3, 4)
  season_label <- paste0(season_start %% 100, "_", season_end)
  
  # Get the variable name
  var_name <- paste0("Player_stats", season_label)
  
  # Save to CSV
  write.csv(get(var_name), 
            file = paste0(var_name, ".csv"), 
            row.names = FALSE)
}

### BEGIN WRANGLING ###

# Add Season column to each dataset before binding, so that we can track which season each statistic came from
Player_stats14_15$Season <- "2014-15"
Player_stats15_16$Season <- "2015-16"
Player_stats16_17$Season <- "2016-17"
Player_stats17_18$Season <- "2017-18"
Player_stats18_19$Season <- "2018-19"
Player_stats19_20$Season <- "2019-20"
Player_stats20_21$Season <- "2020-21"
Player_stats21_22$Season <- "2021-22"
Player_stats22_23$Season <- "2022-23"
Player_stats23_24$Season <- "2023-24"


# Combine all player-season tables vertically to create a master table with all player seasons from the past ten seasons
All_Player_stats14_24 <- bind_rows(
  Player_stats14_15,
  Player_stats15_16,
  Player_stats16_17,
  Player_stats17_18,
  Player_stats18_19,
  Player_stats19_20,
  Player_stats20_21,
  Player_stats21_22,
  Player_stats22_23,
  Player_stats23_24
)

# Save to CSV
write.csv(All_Player_stats14_24, "All_Player_stats14_24.csv", row.names = FALSE)


# List of all your player stats table suffixes - this gets repeated throughout the code a lot for reference (usually above any season-based wrangling)
season_labels <- c("14_15", "15_16", "16_17", "17_18", "18_19",
                   "19_20", "20_21", "21_22", "22_23", "23_24")

for (label in season_labels) {
  
  df <- get(paste0("Player_stats", label))
  
  # Step 1: Identify players with combined team row (e.g., "2TM", "3TM", etc.)
  multi_team_players <- df |>
    filter(str_detect(Team, "TM")) |> 
    select(Player) |> 
    distinct()
  
  # Step 2: Create team list for each player
  team_lists <- df |>
    filter(Player %in% multi_team_players$Player & !str_detect(Team, "TM")) |>
    group_by(Player) |>
    summarise(`Teams Played For` = paste(sort(unique(Team)), collapse = ", "), .groups = "drop")
  
  # Step 3: Keep only the summary rows for multi-team players
  df_filtered <- df |>
    filter(!(Player %in% multi_team_players$Player & !str_detect(Team, "TM"))) |> 
    left_join(team_lists, by = "Player")
  
  # Step 4: Save the filtered dataset
  assign(paste0("Player_stats", label, "_filtered"), df_filtered)
}

# Bind them all together
All_Player_stats14_24_filtered <- bind_rows(
  lapply(season_labels, function(label) {
    get(paste0("Player_stats", label, "_filtered"))
  })
)

# Save to CSV, this now gives us a filtered dataset for all players seasons over the course of the ten year period
write.csv(All_Player_stats14_24_filtered, "All_Player_stats14_24_filtered.csv", row.names = FALSE)



# Load the main filtered dataset
all_stats <- read_csv("All_Player_stats14_24_filtered.csv")

# Remove Rk column, won't matter for this section
all_stats <- all_stats |> select(-Rk)

# Save Age for 23-24 season only, since we can easily calculate age from that number for any other seasons if we need to
age_col <- all_stats |> 
  filter(Season == "2023-24") |> 
  select(Player, Age) |> 
  rename(Age_23_24 = Age)

# Handle Position summary, in case some players switch position or play multiple positions with different teams
pos_summary <- all_stats |> 
  count(Player, Pos) |> 
  group_by(Player) |> 
  summarise(
    All_Positions = paste0(Pos, " (", n, ")", collapse = ", "),
    Majority_Position = Pos[which.max(n)],
    .groups = "drop"
  )

# Handle Team aggregation
team_summary <- all_stats |> 
  mutate(Team_key = ifelse(is.na(`Teams Played For`), Team, paste0("{", `Teams Played For`, "}"))) |> 
  count(Player, Team_key) |> 
  group_by(Player) |> 
  summarise(
    Teams_Played_For = paste0(Team_key, " (", n, ")", collapse = ", "),
    .groups = "drop"
  )

# Reshape stats to wide format
stat_cols <- setdiff(names(all_stats), c("Player", "Age", "Team", "Pos", "Season", "Teams Played For"))

stats_wide <- all_stats |> 
  select(Player, Season, all_of(stat_cols)) |> 
  pivot_wider(
    names_from = Season,
    values_from = all_of(stat_cols),
    names_glue = "{.value}_{Season}"
  )

# Combine everything into the final unique-player table
Unique_Players14_24 <- stats_wide |> 
  left_join(age_col, by = "Player") |> 
  left_join(pos_summary, by = "Player") |> 
  left_join(team_summary, by = "Player")

# Save to CSV, we now have all players that played in the NBA over the past 10 seasons and their stats for each year as desired
write_csv(Unique_Players14_24, "Unique_Players14_24.csv")

# Check if all player names are unique
anyDuplicated(Unique_Players14_24$Player) # returns 0 as desired, so indeed we have everything we need here in terms of unique players

# Now, we'll figure out a method to get player averages over the course of the seasons that they played. Note, not over the course of all ten seasons
# since some players have only played a few years (younger, got to the NBA later, etc).

# Step 1: Define columns to exclude, pretty much all the non-numeric stuff
cols_to_exclude <- c(
  "Player", 
  "All_Positions", 
  "Majority_Position", 
  "Teams_Played_For", 
  "Age_23_24",
  colnames(Unique_Players14_24)[str_starts(colnames(Unique_Players14_24), "Awards_")]
)

# Step 2: Capture original stat columns and extract base stat names
original_stat_cols <- setdiff(colnames(Unique_Players14_24), cols_to_exclude)
ordered_stats <- unique(str_remove(original_stat_cols, "_\\d{4}-\\d{2}$"))

# Step 3: Preserve original player order
original_player_order <- Unique_Players14_24$Player

# Step 4: Compute averages
Unique_player_averages <- Unique_Players14_24 |>
  pivot_longer(cols = all_of(original_stat_cols), names_to = "Stat_Season", values_to = "Value") |>
  separate(Stat_Season, into = c("Stat", "Season"), sep = "_", extra = "merge") |>
  mutate(Stat = factor(Stat, levels = ordered_stats)) |>
  group_by(Player, Stat) |>
  summarise(Average = mean(Value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Stat, values_from = Average)

# Step 5: Restore original player and stat order
Unique_player_averages <- Unique_player_averages |>
  mutate(Player = factor(Player, levels = original_player_order)) |>
  arrange(Player) |>
  select(Player, all_of(ordered_stats))

# Step 6: Save, so now we have player averages over the course of their time (as season averages, not game averages quite yet, although we could get that too).
write_csv(Unique_player_averages, "Unique_player_averages.csv")

# Which players averaged over 1000 field goal attempts each season since they've been playing in the NBA?

Unique_player_averages |>
  filter(FGA >= 1000) |>
  pull(Player)

Unique_player_averages |>
  filter(FGA >= 1000) |>
  summarise(n = n())



# List of season labels (repeat from above, for reference)
season_labels <- c("14_15", "15_16", "16_17", "17_18", "18_19",
                   "19_20", "20_21", "21_22", "22_23", "23_24")

# Function to compute per-game stats
compute_per_game <- function(df) {
  # Identify columns to exclude: non-numeric, percentages, and columns not suitable for per-game calculation
  excluded_cols <- c("Player", "Team", "Pos", "Age", "Season", "Teams Played For", "Awards", "Rk", "G")
  percent_cols <- grep("%", names(df), value = TRUE)
  non_per_game_cols <- union(excluded_cols, percent_cols)
  
  # Identify columns to compute per-game stats for
  stat_cols <- setdiff(names(df), non_per_game_cols)
  
  # Make sure G (Games played) column exists
  if (!"G" %in% names(df)) stop("Column 'G' missing in input")
  
  # Compute per-game stats and rename
  per_game_stats <- df |>
    mutate(across(all_of(stat_cols), ~ .x / G)) |>
    select(all_of(stat_cols)) |> 
    rename_with(~ paste0(
      case_when(
        .x == "PTS" ~ "PPG",
        .x == "AST" ~ "APG",
        .x == "TRB" ~ "RPG",
        .x == "DRB" ~ "DRPG",
        .x == "ORB" ~ "ORPG",
        .x == "STL" ~ "SPG",
        .x == "BLK" ~ "BPG",
        .x == "TOV" ~ "TOPG",
        .x == "PF"  ~ "PFPG",
        .x == "FG"  ~ "FGPG",
        .x == "FGA" ~ "FGAPG",
        .x == "3P"  ~ "3PPG",
        .x == "3PA" ~ "3PAPG",
        .x == "2P"  ~ "2PPG",
        .x == "2PA" ~ "2PAPG",
        .x == "FT"  ~ "FTPG",
        .x == "FTA" ~ "FTAPG",
        .x == "GS"  ~ "GSPG",
        .x == "MP"  ~ "MPG",
        .x == "Trp-Dbl" ~ "TripleDoublePG",
        TRUE ~ paste0(.x, "_per_game")
      )
    ))
  
  # Bind to original dataset
  bind_cols(df, per_game_stats)
}

# Loop through each dataset, compute per-game stats, assign new dataset for each of the seasons, so that those also include per game averages
for (label in season_labels) {
  df <- get(paste0("Player_stats", label, "_filtered"))
  df_pergame <- compute_per_game(df)
  assign(paste0("Player_stats", label, "_filtered_pergame"), df_pergame)
}
