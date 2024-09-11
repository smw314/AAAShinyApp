# Load required libraries
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

# Load datasets
pitcher_data <- read_csv('pitcher_data.csv')   # Data for pitchers
teams <- read_csv('team.csv')                  # List of teams
batting_teams <- read_csv('batting_team.csv')  # List of batting teams
batter_names <- read_csv('batter_names.csv')   # List of batter names
pitcher_names <- read_csv('pitcher_names.csv') # List of pitcher names

# Define UI layout for the Shiny app
shinyUI(fluidPage(
  theme = shinytheme("cerulean"),  # Set theme for the UI
  
  # Main title and author information
  titlePanel("2023 Triple-A and FSL Data"),
  h4("By Sam Wirth @SamWirthSports"),
  h5("Last Updated: 10/01/2023"),
  
  # Navigation panel for different sections of the app
  navlistPanel("Pitchers",  # Tab for pitchers
  
    # Tab for viewing data of all pitchers
    tabPanel("All", 
             pickerInput(  # Dropdown for selecting league
               inputId = "league_pitcher",
               label = "Select League",
               choices = c("FSL", "PCL", "INT"),  # League options
               multiple = TRUE,  # Allow multiple selections
               selected = c("FSL", "PCL", "INT")  # Default selection
             ),
             dataTableOutput("pitcher_table")  # Updated output ID for pitcher data table
    ),
    
    # Tab for viewing data by team
    tabPanel("Team", 
             pickerInput(  # Dropdown for selecting team
               inputId = "team",
               label = "Enter Team", 
               choices = c(teams),  # Choices from loaded team data
               selected = NULL,
               options = list(
                 `live-search` = TRUE  # Enable live search for teams
               )
             ),
             dataTableOutput("pitcher_team_table")  # Updated output ID for team-specific pitcher data
    ),
    
    # Tab for viewing individual pitcher data
    tabPanel("Individual", 
             pickerInput(  # Dropdown for selecting pitcher by name
               inputId = "filter",
               label = "Enter Pitcher Name", 
               choices = c(unique(pitcher_data$filter)),  # Unique names from pitcher data
               selected = NULL,
               options = list(
                 `live-search` = TRUE  # Enable live search for pitcher names
               )
             ),
             
             # Layout for displaying pitcher data table and multiple plots
             fluidRow(
               column(12, dataTableOutput("pitcher_individual_table")),  # Updated output ID for individual pitcher data table
               column(4, plotOutput("pitcher_plot1")),  # Updated plot outputs
               column(4, plotOutput("pitcher_plot2")),
               column(4, plotOutput("pitcher_plot3")),
               column(4, plotOutput("pitcher_plot4")),
               column(4, plotOutput("pitcher_plot5")),
               column(4, plotOutput("pitcher_plot6"))
             )
    ),
    
    'Batters',  # Tab for batters section
    
    # Tab for viewing data of all batters
    tabPanel("All", 
             pickerInput(  # Dropdown for selecting league for batters
               inputId = "league_batter",
               label = "Select League",
               choices = c("FSL", "PCL", "INT"),  # League options
               multiple = TRUE,  # Allow multiple selections
               selected = c("FSL", "PCL", "INT")  # Default selection
             ),
             dataTableOutput("batter_table")  # Updated output ID for batter data table
    ),
    
    # Tab for viewing batter data by team
    tabPanel("Team", 
             pickerInput(  # Dropdown for selecting team for batters
               inputId = "team2",
               label = "Enter Team", 
               choices = c(batting_teams),  # Choices from loaded batting teams
               selected = NULL,
               options = list(
                 `live-search` = TRUE  # Enable live search for batting teams
               )
             ),
             dataTableOutput("batter_team_table")  # Updated output ID for team-specific batter data
    ),
    
    # Tab for viewing individual batter data
    tabPanel("Individual", 
             pickerInput(  # Dropdown for selecting batter by name
               inputId = "filter2",
               label = "Enter Batter Name", 
               choices = c(batter_names),  # Unique names from batter data
               selected = NULL,
               options = list(
                 `live-search` = TRUE  # Enable live search for batter names
               )
             ),
             dataTableOutput("batter_specific_table"),  # Updated output ID for individual batter data
             
             # Layout for displaying batter plots
             fluidRow(
               column(6, plotOutput("batter_plot1")),  # Updated plot output IDs
               column(6, plotOutput("batter_plot2"))
             )
    ),
    
    widths = c(1, 11)  # Adjust widths of the navigation list and content
  )
))
