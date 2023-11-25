library(baseballr)
library(tidyverse)
#install.packages('furrr')
library(furrr)

# figure out which level_ids we need for minor leagues
leagues <- mlb_league(2023)

# From the mlb_league function we observe the minor league season starts on April 5 and
# ends on September 28 so we create a data.frame going through each game
dates <- data.frame(day = rep(seq(as.Date('2023-03-30'), as.Date('2023-10-01'), by = 'days'),
                              times = 1))
# Acquire the minor league game pks to scrape play-by-play
minor_league_game_pk_list <- 1:nrow(dates) %>% 
  purrr::map(function(x) mlb_game_pks(dates$day[x], level_ids = c(11, 14)))

ml_game_pks <- minor_league_game_pk_list %>% bind_rows() %>% 
  dplyr::filter(status.codedGameState == "F", !is.na(game_pk)) %>%
  pull(game_pk)

safe_pbp <- safely(mlb_pbp)

#Acquire the minor league play-by-play data
ml_pbp <- 1:length(ml_game_pks) %>% furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
  map('result') %>% bind_rows()

#Convert to dataframe
ml_pbp <- ml_pbp %>% as.data.frame()

# Use purrr safely package to continue to run code in case of an error

# SEE IF YOU CAN CLEAN THE GAME PKS HERE (EX: NO DAYTONA TORTUGAS, ETC.)


ml_pbp <- ml_pbp %>% 
  filter(home_league_id %in% c(117, 112, 123),
         home_team != "Daytona Tortugas")

write_csv(ml_pbp, 'raw_pbp.csv')

ml_pbp <- read_csv('raw_pbp.csv')

# write this as csv
# then in future days just load in the most recent day and join that with
# this csv

