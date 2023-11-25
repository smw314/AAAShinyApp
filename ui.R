#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
  navlistPanel("Pitchers",
    tabPanel("All", 
             pickerInput(
               inputId = "league_pitcher",
               label = "Select League",
               choices = c("FSL", "PCL", "INT"),
               multiple = TRUE,
               selected = c("FSL", "PCL", "INT")
             ), dataTableOutput("table")),
    tabPanel("Team", 
             pickerInput(
               inputId = "team",
               label = "Enter Team", 
               choices = c(teams),
               selected = NULL,
               options = list(
                 `live-search` = TRUE)),
             # selectInput("team", "Enter Team", choices = c(unique(ml_pbp$team))),
             dataTableOutput("teamtable")),
    tabPanel("Individual", 
             pickerInput(
               inputId = "filter",
               label = "Enter Pitcher Name", 
               choices = c(unique(pitcher_data$filter)),
               selected = NULL,
               options = list(
                 `live-search` = TRUE)),
             # selectInput("filter", "Enter Pitcher Name", choices = c(unique(ml_pbp$matchup.pitcher.fullName))),
             fluidRow(column(12, dataTableOutput("table2")),
                      column(4, plotOutput("plot")),
                      column(4, plotOutput("plot3")),
                      column(4, plotOutput("plot4")),
                      column(4, plotOutput("plot5")),
                      column(4, plotOutput("plot6")),
                      column(4, plotOutput("plot7")))),
    'Batters',
    tabPanel("All", 
             pickerInput(
               inputId = "league_batter",
               label = "Select League",
               choices = c("FSL", "PCL", "INT"),
               multiple = TRUE,
               selected = c("FSL", "PCL", "INT") # Default to all leagues
             ),
             dataTableOutput("table3")),
    tabPanel("Team",
             pickerInput(
               inputId = "team2",
               label = "Enter Team", 
               choices = c(batting_teams),
               selected = NULL,
               options = list(
                 `live-search` = TRUE)),
             # selectInput("team2", "Enter Team", choices = c(unique(ml_pbp$batting_team))),
             dataTableOutput("teamtable2")),
    tabPanel("Individual",
             pickerInput(
               inputId = "filter2",
               label = "Enter Batter Name", 
               choices = c(batter_names),
               selected = NULL,
               options = list(
                 `live-search` = TRUE)),
             # selectInput("filter2", "Enter Batter Name", choices = c(unique(ml_pbp$matchup.batter.fullName))),
             dataTableOutput("table4"),
             fluidRow(column(6, plotOutput("plot2")),
                      column(6, plotOutput("plot99")))),
    widths = c(1, 11)
  )
)
)
