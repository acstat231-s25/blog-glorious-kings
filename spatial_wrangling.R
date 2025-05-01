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
paths_allowed("https://basketball.realgm.com/player/LeBron-James/GameLogs/250")

### INITIAL DATA SCRAPING ###

# Define the range of years (all years that LeBron James has played)
start_year <- 2025
end_year <- 2003

### LeBron Game Logs ###

# Define destination folder
dest_folder <- "data/game-logs/"

# Make sure the folder exists
if (!dir.exists(dest_folder)) {
  dir.create(dest_folder, recursive = TRUE)
}

# Function to scrape and save one season
scrape_and_save_season <- function(season_end_year) {
  # Create the URL
  url <- paste0(
    "https://basketball.realgm.com/player/LeBron-James/GameLogs/250/NBA/",
    season_end_year,
    "/Reg"
  )
  
  # Try reading the page (some seasons might be missing if he didn't play, but in LeBron's case all should exist)
  page <- tryCatch(read_html(url), error = function(e) NULL)
  
  if (!is.null(page)) {
    # Find the table
    table_node <- html_node(page, "table")
    
    # Only proceed if table exists
    if (!is.na(table_node)) {
      game_log <- html_table(table_node, fill = TRUE)
      
      # Save CSV with season_end_year in the filename
      file_name <- paste0(dest_folder, "LeBron_GameLog_", season_end_year, ".csv")
      write_csv(game_log, file_name)
      
      cat("Saved season:", season_end_year, "\n")
    } else {
      cat("No table found for season:", season_end_year, "\n")
    }
  } else {
    cat("Failed to load page for season:", season_end_year, "\n")
  }
}

# Loop from 2025 back to 2004
for (year in 2025:2004) {
  scrape_and_save_season(year)
}

### Team Game Logs ###

# Note, recall that LeBron played for Cavs 2003-2010, Heat 2010-2014, Cavs 2014-2018, Lakers 2018-2025, so we scrape each team like that

# Destination folder
dest_folder <- "data/team-game-logs/"

# Create folder if doesn't exist
if (!dir.exists(dest_folder)) {
  dir.create(dest_folder, recursive = TRUE)
}

# Define a vector for all season end years: 2004 to 2025
seasons <- 2004:2025

# Define function to get team abbreviation for each year
get_team <- function(year) {
  if (year >= 2004 && year <= 2010) {
    return("CLE")  # Cavaliers first stint
  } else if (year >= 2011 && year <= 2014) {
    return("MIA")  # Heat
  } else if (year >= 2015 && year <= 2018) {
    return("CLE")  # Cavaliers second stint
  } else if (year >= 2019 && year <= 2025) {
    return("LAL")  # Lakers
  } else {
    stop("Year out of LeBron's career range")
  }
}

# Loop over seasons
for (year in seasons) {
  
  # Get the correct team for this year
  team_abbr <- get_team(year)
  
  # Build the URL
  url <- paste0("https://www.basketball-reference.com/teams/", team_abbr, "/", year, "/gamelog/")
  
  # Scrape the tables
  tables <- url |>
    read_html() |>
    html_elements("table")
  
  # Try to extract the first table if exists
  if (length(tables) > 0) {
    team_log <- tables |>
      pluck(1) |>
      html_table(fill = TRUE)
    
    # Save CSV
    file_name <- paste0(dest_folder, "TeamGameLog_", team_abbr, "_", year, ".csv")
    write_csv(team_log, file_name)
    
    cat("Saved:", file_name, "\n")
  } else {
    cat("No table found for:", team_abbr, "season ending", year, "\n")
  }
}

### Unified Logs with Home and Away, and Current Team ###

# Read LeBron's 2025 game log
lebron_2025 <- read_csv("data/game-logs/LeBron_GameLog_2025.csv")

# Read Lakers' 2025 team game log
team_2025 <- read_csv("data/team-game-logs/TeamGameLog_LAL_2025.csv")

# View the column names
colnames(lebron_2025)
colnames(team_2025)

# Make sure dates can match by cleaning up both
# (assuming the 'Date' column is what links them)

# Optional: check column names if needed
# colnames(lebron_2025)
# colnames(team_2025)

### COMBINING EVERYTHING ### (So that we have home AND away represented, as well as the team LeBron played for each season, for spatial data).

# Load packages
library(tidyverse)
library(lubridate)

# Create output folder if it doesn't exist
combined_folder <- "data/combined/"
if (!dir.exists(combined_folder)) {
  dir.create(combined_folder, recursive = TRUE)
}

# Function to get team based on season
get_team <- function(year) {
  if (year >= 2004 && year <= 2010) {
    return("CLE")
  } else if (year >= 2011 && year <= 2014) {
    return("MIA")
  } else if (year >= 2015 && year <= 2018) {
    return("CLE")
  } else if (year >= 2019 && year <= 2025) {
    return("LAL")
  } else {
    stop("Year out of LeBron's career range")
  }
}

# Loop over all seasons
for (year in 2004:2025) {
  # === File paths ===
  team_abbr <- get_team(year)
  lebron_path <- paste0("data/game-logs/LeBron_GameLog_", year, ".csv")
  team_path <- paste0("data/team-game-logs/TeamGameLog_", team_abbr, "_", year, ".csv")
  output_path <- paste0("data/combined/LeBron_GameLog_", year, "_WithTeamInfo.csv")
  
  # === Try-catch to continue even if a file is missing or corrupt ===
  tryCatch({
    # Read data
    lebron_df <- read_csv(lebron_path)
    team_df <- read_csv(team_path, skip = 1)
    
    # Clean LeBron dates
    lebron_df <- lebron_df %>%
      mutate(Date = mdy(Date)) %>%
      filter(!is.na(Date))
    
    # Clean team dates and determine Home/Away
    team_df <- team_df %>%
      filter(!is.na(Date)) %>%
      mutate(
        Date = ymd(Date),
        `Home/Away` = ifelse(`...4` == "@", "Away", NA),
        `Home/Away` = replace_na(`Home/Away`, "Home"),
        Team = team_abbr
      ) %>%
      select(Date, `Home/Away`, Team)
    
    # Merge
    merged_df <- lebron_df %>%
      left_join(team_df, by = "Date")
    
    # Save
    write_csv(merged_df, output_path)
    cat("Saved:", output_path, "\n")
  }, error = function(e) {
    cat("Failed for year", year, ":", conditionMessage(e), "\n")
  })
}

### TEAMS BY STATE ###

### STATE MAP ###

# Load packages
library(tidyverse)
library(lubridate)
library(maps)
library(ggplot2)

# === Load data ===
lebron <- read_csv("data/combined/LeBron_GameLog_2025_WithTeamInfo.csv")
team_states <- read_csv("data/NBA_Teams_By_State.csv")

# === Expand team_states to long format (one team per row) ===
team_states_long <- team_states %>%
  separate_rows(Teams, sep = ",") %>%
  mutate(
    Teams = str_trim(Teams),
    State = str_to_title(State)
  )

# === Create opponent → state lookup table ===
team_lookup <- team_states_long %>%
  distinct(Teams, State) %>%
  rename(Opponent = Teams, OpponentState = State)

# === Clean LeBron data and join on opponent team ===
lebron <- lebron %>%
  mutate(
    Opponent = str_trim(Opponent),
    GameState = if_else(`Home/Away` == "Home", "California", NA)
  ) %>%
  left_join(team_lookup, by = "Opponent") %>%
  mutate(
    GameState = if_else(`Home/Away` == "Away", OpponentState, GameState)
  )

# === Summarize total points by state ===
points_by_state <- lebron %>%
  filter(!is.na(GameState)) %>%
  group_by(GameState) %>%
  summarise(TotalPoints = sum(PTS, na.rm = TRUE)) %>%
  mutate(region = str_to_lower(GameState))

# === Get U.S. map data ===
states_map <- map_data("state")

# Join map data with point totals
map_data_joined <- left_join(states_map, points_by_state, by = "region")

# === Plot ===
ggplot(map_data_joined, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = TotalPoints), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "LeBron James: Total Points by State (2025 Season)",
    fill = "Total Points",
    caption = "Home games = California; Away games matched to opponent team state"
  ) +
  coord_fixed(1.3)




