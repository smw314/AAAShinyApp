library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(rsconnect)
library(plumber)
library(readr)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # Load data files
  pitcher_data <- read_csv('pitcher_data.csv')
  batter_data <- read_csv('batter_data.csv')
  pitcher_game_data <- read_csv('pitcher_data_2.csv')
  pitcher_usage_data <- read_csv('pitcher_data_3.csv')
  pitch_data <- read_csv('pitch_data.csv')
  batted_ball_data <- read_csv('bbe_data.csv')

  # Function to create spray chart
  create_spray_chart <- function(...) { 
    ggplot(...) +
      geom_curve(x = 33, xend = 223, y = -100, yend = -100, curvature = -.65, color = "black") + 
      geom_segment(x = 128, xend = 33, y = -208, yend = -100, color = "black") + 
      geom_segment(x = 128, xend = 223, y = -208, yend = -100, color = "black") +
      geom_curve(x = 83, xend = 173, y = -155, yend = -156, curvature = -.65, 
                 linetype = "dotted", color = "black") +
      coord_fixed() + scale_x_continuous(NULL, limits = c(25, 225)) + 
      scale_y_continuous(NULL, limits = c(-225, -25))
  }

  # Reactive expression for filtered pitcher data
  filtered_pitcher_data <- reactive({
    pitcher_data %>% 
      filter(league %in% input$league_pitcher) %>%
      select(-filter) %>%
      setNames(c("Name", "Age", "Team", "League", "Pitch Type", "Pitches", 
                 "Velocity", "IVB", "HB", "Spin Rate", "Spin Direction", 
                 "VAA", "Average EV", "OZ%", "IZ%", "SwStr%", "Whiff%",
                 "CSW%", "GB%"))
  })

  # Reactive expression for individual pitcher data
  individual_pitcher_data <- reactive({
    pitcher_data %>% 
      filter(filter == input$filter) %>% 
      select(-filter, -team, -league) %>%
      setNames(c("Name", "Age", "Pitch Type", "Pitches", 
                 "Velocity", "IVB", "HB", "Spin Rate", "Spin Direction", 
                 "VAA", "Average EV", "OZ%", "IZ%", "SwStr%", "Whiff%",
                 "CSW%", "GB%"))
  })

  # Reactive expression for pitcher pitch data
  pitcher_pitch_data <- reactive({
    pitch_data %>% 
      filter(filter == input$filter) %>% 
      select(-filter)
  })

  # Reactive expression for team pitcher data
  team_pitcher_data <- reactive({
    pitcher_data %>% 
      filter(team == input$team) %>% 
      select(-team, -league, -filter) %>%
      setNames(c("Name", "Age", "Pitch Type", "Pitches", 
                 "Velocity", "IVB", "HB", "Spin Rate", "Spin Direction", 
                 "VAA", "Average EV", "OZ%", "IZ%", "SwStr%", "Whiff%", 
                 "CSW%", "GB%"))
  })

  # Reactive expressions for pitcher game data and usage data
  pitcher_game_data_filtered <- reactive({
    pitcher_game_data %>% 
      filter(filter == input$filter)
  })

  pitcher_usage_data_filtered <- reactive({
    pitcher_usage_data %>% 
      filter(filter == input$filter)
  })

  # Reactive expression for filtered batter data
  filtered_batter_data <- reactive({
    batter_data %>% 
      filter(league %in% input$league_batter) %>%
      setNames(c("Name", "Age", "Team", "League", "Pitches", "BBE", "Max EV",
                 "95th Pct. EV", "Average EV", "LA Sweet Spot%",
                 "LA Standard Deviation", "OZ-Swing%", "IZ-Contact%", "Swing%" ,
                 "SwStr%", "Whiff%", "Hard Hit%", "Barrel%", "FB%",
                 "FB SwStr%", "BB%", "BB SwStr%", "OFF%", "OFF SwStr%"))
  })

  # Reactive expression for team batter data
  team_batter_data <- reactive({
    batter_data %>% 
      filter(batting_team == input$team2) %>% 
      select(-batting_team, -league) %>%
      setNames(c("Name", "Age", "Pitches", "BBE", "Max EV",
                 "95th Pct. EV", "Average EV", "LA Sweet Spot%",
                 "LA Standard Deviation",
                 "OZ-Swing%", "IZ-Contact%", "Swing%", "SwStr%", "Whiff%", "Hard Hit%", "Barrel%",
                 "FB%", "FB SwStr%", "BB%", "BB SwStr%", "OFF%", "OFF SwStr%"))
  })

  # Reactive expression for individual batter data
  individual_batter_data <- reactive({
    batter_data %>% 
      filter(matchup.batter.fullName == input$filter2) %>%
      setNames(c("Name", "Age", "Team", "League", "Pitches", "BBE", "Max EV",
                 "95th Pct. EV", "Average EV", "LA Sweet Spot%",
                 "LA Standard Deviation",
                 "OZ-Swing%", "IZ-Contact%", "Swing%", "SwStr%", "Whiff%", "Hard Hit%", "Barrel%",
                 "FB%", "FB SwStr%", "BB%", "BB SwStr%", "OFF%", "OFF SwStr%"))
  })

  # Reactive expression for individual batter's batted ball data
  individual_batter_bbe_data <- reactive({
    batted_ball_data %>% 
      filter(matchup.batter.fullName == input$filter2)
  })

  # Render data tables
  output$pitcher_table <- DT::renderDataTable({
    filtered_pitcher_data()
  }, filter = "top",
  extensions = "FixedColumns", 
  rownames = FALSE,
  options = list(
    scrollX = TRUE, 
    scrollY = "500px",
    fixedColumns = list(leftColumns = 1)
  ))

  output$team_pitcher_table <- DT::renderDataTable({
    team_pitcher_data()
  }, filter = "top",
  extensions = "FixedColumns", 
  rownames = FALSE,
  options = list(
    scrollX = TRUE, 
    scrollY = "500px",
    fixedColumns = list(leftColumns = 1)
  ))

  output$individual_pitcher_table <- renderDataTable({
    individual_pitcher_data()
  }, filter = "top")

  output$batter_table <- renderDataTable({
    filtered_batter_data()
  }, filter = "top",
  extensions = "FixedColumns", 
  rownames = FALSE,
  options = list(
    scrollX = TRUE, 
    scrollY = "500px",
    fixedColumns = list(leftColumns = 1)
  ))

  output$individual_batter_table <- renderDataTable({
    individual_batter_data()
  })

  output$team_batter_table <- DT::renderDataTable({
    team_batter_data()
  }, filter = "top",
  extensions = "FixedColumns", 
  rownames = FALSE,
  options = list(
    scrollX = TRUE, 
    scrollY = "500px",
    fixedColumns = list(leftColumns = 1)
  ))

  # Render plots
  output$batter_spray_chart <- renderPlot({
    create_spray_chart(individual_batter_bbe_data(), 
                       aes(x = hitData.coordinates.coordX, 
                           y = -hitData.coordinates.coordY, 
                           color = hitData.launchSpeed)) + 
      geom_point(size = 3) +
      scale_color_gradient(low="blue", high="red") +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank()) +
      labs(colour="Exit Velocity") +
      geom_point(data = subset(individual_batter_bbe_data(), hitData.launchSpeed > 99.9), 
                 shape=17, size=5, na.rm = TRUE)
  })

  output$batter_launch_angle_plot <- renderPlot({
    ggplot(data = subset(individual_batter_bbe_data(), !is.na(hitData.launchAngle)), 
           aes(x = hitData.launchAngle)) +
      geom_density() +
      ggtitle("Launch Angle Density Plot") +
      xlab("Launch Angle") + ylab("Density") +
      geom_vline(xintercept = 8, color = "red") +
      geom_vline(xintercept = 32, color = "red")
  })

  output$pitcher_movement_plot <- renderPlot({
    ggplot(data = subset(pitcher_pitch_data(), !is.na(details.type.description)), 
           aes(x = hb, y = ivb, 
               color = details.type.description)) +
      geom_point(size = 3, alpha = .8) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      ggtitle("Pitch Movement Profile") +
      xlab("HB") + ylab("IVB") +
      guides(color = guide_legend(title = "Pitch"))
  }, height = 350, width = 400)

  output$pitcher_location_plot <- renderPlot({
    ggplot(data = subset(pitcher_pitch_data(), !is.na(details.type.description)),
           aes(x = pitchData.coordinates.pX, y = pitchData.coordinates.pZ, 
               color = details.type.description)) +
      geom_point(size = 3, alpha = .8) +
      ggtitle("Pitch Location") +
      xlab("Pitch Location (x)") + ylab("Pitch Location (y)") +
      geom_rect(xmin = -(17/2)/12,
                xmax = (17/2)/12,
                ymin = 1.5, 
                ymax = 3.6, 
                alpha=0.0001,
                color = "black") +
      xlim(-2, 2) +
      ylim(0, 6) +
      guides(color = guide_legend(title = "Pitch"))
  }, height = 350, width = 400)

  output$pitcher_velocity_plot <- renderPlot({
    ggplot(data = subset(pitcher_game_data_filtered(), !is.na(details.type.description)), 
           aes(x = game_date, y = velo, color = details.type.description, group = details.type.description)) +
      geom_point() +
      geom_line() +
      ggtitle(paste("Velocity by Appearance")) +
      guides(color = guide_legend(title = "Pitch")) +
      ylab('Velocity') +
      xlab("Appearance Date")
  }, height = 350, width = 400)

  output$pitcher_usage_plot <- renderPlot({
    ggplot(data = subset(pitcher_usage_data_filtered(), !is.na(details.type.description)), 
           aes(x = game_date, y = pitch_percentage, color = details.type.description, group = details.type.description)) +
      geom_point() +
      geom_line() +
      ggtitle(paste("Pitch Usage by Appearance")) +
      guides(color = guide_legend(title = "Pitch")) +
      ylab('Usage') +
      xlab("Appearance Date")
  }, height = 350, width = 400)

  output$pitcher_ivb_plot <- renderPlot({
    ggplot(data = subset(pitcher_game_data_filtered(), !is.na(details.type.description)), 
           aes(x = game_date, y = pfx_z, color = details.type.description, group = details.type.description)) +
      geom_point() +
      geom_line() +
      ggtitle(paste("IVB by Appearance")) +
      guides(color = guide_legend(title = "Pitch")) +
      ylab('IVB') +
      xlab("Appearance Date")
  }, height = 350, width = 400)

  output$pitcher_hb_plot <- renderPlot({
    ggplot(data = subset(pitcher_game_data_filtered(), !is.na(details.type.description)), 
           aes(x = game_date, y = pfx_x, color = details.type.description, group = details.type.description)) +
      geom_point() +
      geom_line() +
      ggtitle(paste("HB by Appearance")) +
      guides(color = guide_legend(title = "Pitch")) +
      ylab('HB') +
      xlab("Appearance Date")
  }, height = 350, width = 400)

})
