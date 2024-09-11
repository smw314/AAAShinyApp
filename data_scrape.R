# Load required libraries
library(baseballr)
library(tidyverse)
library(furrr)

# Get minor league information
minor_league_info <- mlb_league(2023)

# Create date range for the minor league season (March 30 to October 1, 2023)
season_dates <- data.frame(date = seq(as.Date('2023-03-30'), as.Date('2023-10-01'), by = 'days'))

# Function to safely get play-by-play data
safe_get_pbp <- safely(mlb_pbp)

# Get game IDs for minor league games
minor_league_game_ids <- 1:nrow(season_dates) %>%
  purrr::map(function(x) mlb_game_pks(season_dates$date[x], level_ids = c(11, 14))) %>%
  bind_rows() %>%
  filter(status.codedGameState == "F", !is.na(game_pk)) %>%
  pull(game_pk)

# Fetch play-by-play data for all minor league games
minor_league_pbp_data <- 1:length(minor_league_game_ids) %>%
  furrr::future_map(function(x) safe_get_pbp(minor_league_game_ids[x]), .progress = TRUE) %>%
  map('result') %>%
  bind_rows() %>%
  as.data.frame()

# Filter out unwanted teams and leagues
filtered_minor_league_pbp <- minor_league_pbp_data %>%
  filter(home_league_id %in% c(117, 112, 123), home_team != "Daytona Tortugas")

# Save the filtered data
write_csv(filtered_minor_league_pbp, 'raw_pbp.csv')

# For future updates, load the existing data and append new data:
# existing_data <- read_csv('raw_pbp.csv')
# new_data <- get_new_pbp_data()  # Function to get only new data
# updated_data <- bind_rows(existing_data, new_data)
# write_csv(updated_data, 'raw_pbp.csv')
