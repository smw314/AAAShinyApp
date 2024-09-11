library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(readr)

shinyServer(function(input, output, session) {
  
  # Load the data
  pitcher_data <- read_csv('pitcher_data.csv')
  batter_data <- read_csv('batter_data.csv')
  pitcher_velocity_data <- read_csv('pitcher_data_2.csv')
  pitcher_usage_data <- read_csv('pitcher_data_3.csv')
  pitch_movement_data <- read_csv('pitch_data.csv')
  batted_ball_data <- read_csv('bbe_data.csv')
  
  # Function to generate the spray chart layout for hit data
  spray_chart_layout <- function(...) { 
    ggplot(...) +
      geom_curve(x = 33, xend = 223, y = -100, yend = -100, curvature = -.65, color = "black") + 
      geom_segment(x = 128, xend = 33, y = -208, yend = -100, color = "black") + 
      geom_segment(x = 128, xend = 223, y = -208, yend = -100, color = "black") +
      geom_curve(x = 83, xend = 173, y = -155, yend = -156, curvature = -.65, 
                 linetype = "dotted", color = "black") +
      coord_fixed() + 
      scale_x_continuous(NULL, limits = c(25, 225)) + 
      scale_y_continuous(NULL, limits = c(-225, -25))
  }
  
  # Reactive filtering for pitcher data based on the selected league
  filtered_pitcher_data <- reactive({
    pitcher_data %>% 
      filter(league %in% input$league_pitcher) %>%
      select(-filter) %>%
      rename(
        Name = name, Age = age, Team = team, League = league, 
        `Pitch Type` = pitch_type, Pitches = pitches, Velocity = velocity, 
        IVB = ivb, HB = hb, `Spin Rate` = spin_rate, `Spin Direction` = spin_direction, 
        VAA = vaa, `Average EV` = avg_ev, `OZ%` = oz_percent, `IZ%` = iz_percent, 
        `SwStr%` = swstr_percent, `Whiff%` = whiff_percent, `CSW%` = csw_percent, 
        `GB%` = gb_percent
      )
  })

  # Reactive filtering for specific pitcher attributes based on the selected filter
  filtered_pitcher_by_filter <- reactive({
    pitcher_data %>% 
      filter(filter == input$filter) %>%
      select(-filter, -team, -league) %>%
      rename(
        Name = name, Age = age, `Pitch Type` = pitch_type, Pitches = pitches, 
        Velocity = velocity, IVB = ivb, HB = hb, `Spin Rate` = spin_rate, 
        `Spin Direction` = spin_direction, VAA = vaa, `Average EV` = avg_ev, 
        `OZ%` = oz_percent, `IZ%` = iz_percent, `SwStr%` = swstr_percent, 
        `Whiff%` = whiff_percent, `CSW%` = csw_percent, `GB%` = gb_percent
      )
  })

  # Reactive filtering for pitch movement data
  filtered_pitch_movement <- reactive({
    pitch_movement_data %>% 
      filter(filter == input$filter) %>%
      select(-filter)
  })
  
  # Reactive filtering for pitcher data by team
  filtered_pitcher_by_team <- reactive({
    pitcher_data %>% 
      filter(team == input$team) %>%
      select(-team, -league, -filter) %>%
      rename(
        Name = name, Age = age, `Pitch Type` = pitch_type, Pitches = pitches, 
        Velocity = velocity, IVB = ivb, HB = hb, `Spin Rate` = spin_rate, 
        `Spin Direction` = spin_direction, VAA = vaa, `Average EV` = avg_ev, 
        `OZ%` = oz_percent, `IZ%` = iz_percent, `SwStr%` = swstr_percent, 
        `Whiff%` = whiff_percent, `CSW%` = csw_percent, `GB%` = gb_percent
      )
  })
  
  # Reactive filtering for velocity data
  filtered_velocity <- reactive({
    pitcher_velocity_data %>% 
      filter(filter == input$filter)
  })
  
  # Reactive filtering for pitch usage data
  filtered_usage <- reactive({
    pitcher_usage_data %>% 
      filter(filter == input$filter)
  })
  
  # Reactive filtering for batter data by league
  filtered_batter_data <- reactive({
    batter_data %>% 
      filter(league %in% input$league_batter) %>%
      rename(
        Name = name, Age = age, Team = team, League = league, Pitches = pitches, 
        BBE = bbe, `Max EV` = max_ev, `95th Pct. EV` = ev_95_pct, 
        `Average EV` = avg_ev, `LA Sweet Spot%` = la_sweet_spot, 
        `LA Std Dev` = la_std_dev, `OZ-Swing%` = oz_swing, `IZ-Contact%` = iz_contact, 
        `Swing%` = swing_percent, `SwStr%` = swstr_percent, `Whiff%` = whiff_percent, 
        `Hard Hit%` = hard_hit_percent, `Barrel%` = barrel_percent, `FB%` = fb_percent, 
        `FB SwStr%` = fb_swstr_percent, `BB%` = bb_percent, `BB SwStr%` = bb_swstr_percent, 
        `OFF%` = off_percent, `OFF SwStr%` = off_swstr_percent
      )
  })
  
  # Reactive filtering for batter data by team
  filtered_batter_by_team <- reactive({
    batter_data %>% 
      filter(batting_team == input$team2) %>%
      rename(
        Name = name, Age = age, Pitches = pitches, BBE = bbe, `Max EV` = max_ev, 
        `95th Pct. EV` = ev_95_pct, `Average EV` = avg_ev, `LA Sweet Spot%` = la_sweet_spot, 
        `LA Std Dev` = la_std_dev, `OZ-Swing%` = oz_swing, `IZ-Contact%` = iz_contact, 
        `Swing%` = swing_percent, `SwStr%` = swstr_percent, `Whiff%` = whiff_percent, 
        `Hard Hit%` = hard_hit_percent, `Barrel%` = barrel_percent, `FB%` = fb_percent, 
        `FB SwStr%` = fb_swstr_percent, `BB%` = bb_percent, `BB SwStr%` = bb_swstr_percent, 
        `OFF%` = off_percent, `OFF SwStr%` = off_swstr_percent
      )
  })
  
  # Reactive filtering for batter-specific data
  filtered_batter_specific <- reactive({
    batter_data %>% 
      filter(matchup.batter.fullName == input$filter2) %>%
      rename(
        Name = name, Age = age, Team = team, League = league, Pitches = pitches, 
        BBE = bbe, `Max EV` = max_ev, `95th Pct. EV` = ev_95_pct, `Average EV` = avg_ev, 
        `LA Sweet Spot%` = la_sweet_spot, `LA Std Dev` = la_std_dev, `OZ-Swing%` = oz_swing, 
        `IZ-Contact%` = iz_contact, `Swing%` = swing_percent, `SwStr%` = swstr_percent, 
        `Whiff%` = whiff_percent, `Hard Hit%` = hard_hit_percent, `Barrel%` = barrel_percent, 
        `FB%` = fb_percent, `FB SwStr%` = fb_swstr_percent, `BB%` = bb_percent, 
        `BB SwStr%` = bb_swstr_percent, `OFF%` = off_percent, `OFF SwStr%` = off_swstr_percent
      )
  })
  
  # Render the tables and plots in the UI
  output$pitcher_table <- DT::renderDataTable({
    filtered_pitcher_data()
  }, filter = "top", extensions = "FixedColumns", rownames = FALSE, options = list(
    scrollX = TRUE, scrollY = "500px", fixedColumns = list(leftColumns = 1)
  ))
  
  output$team_pitcher_table <- DT::renderDataTable({
    filtered_pitcher_by_team()
  }, filter = "top", extensions = "FixedColumns", rownames = FALSE, options = list(
    scrollX = TRUE, scrollY = "500px", fixedColumns = list(leftColumns = 1)
  ))
  
  output$filtered_pitcher_table <- DT::renderDataTable({
    filtered_pitcher_by_filter()
  }, filter = "top", extensions = "FixedColumns", rownames = FALSE, options = list(
    scrollX = TRUE, scrollY = "500px", fixedColumns = list(leftColumns = 1)
  ))

  output$batter_table <- DT::renderDataTable({
    filtered_batter_data()
  }, filter = "top", extensions = "FixedColumns", rownames = FALSE, options = list(
    scrollX = TRUE, scrollY = "500px", fixedColumns = list(leftColumns = 1)
  ))
  
  output$batter_team_table <- DT::renderDataTable({
    filtered_batter_by_team()
  }, filter = "top", extensions = "FixedColumns", rownames = FALSE, options = list(
    scrollX = TRUE, scrollY = "500px", fixedColumns = list(leftColumns = 1)
  ))
  
  output$batter_specific_table <- DT::renderDataTable({
    filtered_batter_specific()
  }, filter = "top", extensions = "FixedColumns", rownames = FALSE, options = list(
    scrollX = TRUE, scrollY = "500px", fixedColumns = list(leftColumns = 1)
  ))

})
