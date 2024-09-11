library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(rsconnect)
library(plumber)
library(readr)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)

pitcher_data <- read_csv('pitcher_data.csv')
teams <- read_csv('team.csv')
batting_teams <- read_csv('batting_team.csv')
batter_names <- read_csv('batter_names.csv')
pitcher_names <- read_csv('pitcher_names.csv')

shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("2023 Triple-A and FSL Data"),
  h4("By Sam Wirth @SamWirthSports"),
  h5("Last Updated: 10/01/2023"),
  navlistPanel(
    "Pitchers",
    tabPanel("All", 
             pickerInput(
               inputId = "league_pitcher",
               label = "Select League",
               choices = c("FSL", "PCL", "INT"),
               multiple = TRUE,
               selected = c("FSL", "PCL", "INT")
             ),
             dataTableOutput("pitcher_table")
    ),
    tabPanel("Team", 
             pickerInput(
               inputId = "team",
               label = "Enter Team",
               choices = c(teams),
               selected = NULL,
               options = list(`live-search` = TRUE)
             ),
             dataTableOutput("team_pitcher_table")
    ),
    tabPanel("Individual", 
             pickerInput(
               inputId = "filter",
               label = "Enter Pitcher Name",
               choices = c(unique(pitcher_data$filter)),
               selected = NULL,
               options = list(`live-search` = TRUE)
             ),
             fluidRow(
               column(12, dataTableOutput("individual_pitcher_table")),
               column(4, plotOutput("pitcher_movement_plot")),
               column(4, plotOutput("pitcher_location_plot")),
               column(4, plotOutput("pitcher_velocity_plot")),
               column(4, plotOutput("pitcher_usage_plot")),
               column(4, plotOutput("pitcher_ivb_plot")),
               column(4, plotOutput("pitcher_hb_plot"))
             )
    ),
    'Batters',
    tabPanel("All", 
             pickerInput(
               inputId = "league_batter",
               label = "Select League",
               choices = c("FSL", "PCL", "INT"),
               multiple = TRUE,
               selected = c("FSL", "PCL", "INT")
             ),
             dataTableOutput("batter_table")
    ),
    tabPanel("Team", 
             pickerInput(
               inputId = "team2",
               label = "Enter Team",
               choices = c(batting_teams),
               selected = NULL,
               options = list(`live-search` = TRUE)
             ),
             dataTableOutput("team_batter_table")
    ),
    tabPanel("Individual", 
             pickerInput(
               inputId = "filter2",
               label = "Enter Batter Name",
               choices = c(batter_names),
               selected = NULL,
               options = list(`live-search` = TRUE)
             ),
             dataTableOutput("individual_batter_table"),
             fluidRow(
               column(6, plotOutput("batter_spray_chart")),
               column(6, plotOutput("batter_launch_angle_plot"))
             )
    ),
    widths = c(1, 11)
  )
))
