#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(shiny)
library(rsconnect)
library(plumber)
library(readr)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  pitcher_data <- read_csv('pitcher_data.csv')
  batter_data <- read_csv('batter_data.csv')
  pitcher_data_2 <- read_csv('pitcher_data_2.csv')
  pitcher_data_3 <- read_csv('pitcher_data_3.csv')
  pitch_data <- read_csv('pitch_data.csv')
  bbe_data <- read_csv('bbe_data.csv')
  
  spray_chart <- function(...) { 
    ggplot(...) +
      geom_curve(x = 33, xend = 223, y = -100, yend = -100, curvature = -.65, color = "black") + 
      geom_segment(x = 128, xend = 33, y = -208, yend = -100, color = "black") + 
      geom_segment(x = 128, xend = 223, y = -208, yend = -100, color = "black") +
      geom_curve(x = 83, xend = 173, y = -155, yend = -156, curvature = -.65, 
                 linetype = "dotted", color = "black") +
      coord_fixed() + scale_x_continuous(NULL, limits = c(25, 225)) + 
      scale_y_continuous(NULL, limits = c(-225, -25))
  }
  
  filtered <- reactive({
    pitcher_data <- pitcher_data %>% 
      filter(league %in% input$league_pitcher) %>%
      select(-filter)
    colnames(pitcher_data) <- c("Name", "Age", "Team", "League", "Pitch Type", "Pitches", 
                                "Velocity", "IVB", 
                                "HB", "Spin Rate", "Spin Direction", 
                                "VAA",
                                "Average EV", "OZ%", "IZ%", "SwStr%", "Whiff%",
                                "CSW%",
                                "GB%")
    
    pitchers <- na.omit(pitcher_data)
    
  })
  
  
  filtered2 <- reactive({
    pitcher <- pitcher_data %>% 
      filter(filter == input$filter) %>% 
      select(-filter, -team, -league)
    
    colnames(pitcher) <- c("Name", "Age", "Pitch Type", "Pitches", 
                                "Velocity", "IVB", 
                                "HB", "Spin Rate", "Spin Direction", 
                           "VAA",
                                "Average EV", "OZ%", "IZ%", "SwStr%", "Whiff%",
                           "CSW%",
                                "GB%")
    
    
    pitcher <- pitcher
    
  })

  filtered3 <- reactive({
    pitcher <- pitch_data %>% 
      filter(filter == input$filter) %>% 
      select(-filter)
    
  })
  
  team <- reactive({
    
    pitcher <- pitcher_data %>% 
      filter(team == input$team) %>% 
      select(-team, -league, -filter)
    
    colnames(pitcher) <- c("Name", "Age", "Pitch Type", "Pitches", 
                                "Velocity", "IVB", 
                                "HB", "Spin Rate", "Spin Direction", 
                           "VAA",
                                "Average EV", "OZ%", "IZ%", "SwStr%", "Whiff%", 
                           "CSW%",
                                "GB%")
    
    pitcher <- pitcher
  })
  
  
  filtered8 <- reactive({
    pitcher <- pitcher_data_2 %>% 
      filter(filter == input$filter)
  })
  
  filtered9 <- reactive({
    pitcher <- pitcher_data_3 %>% 
      filter(filter == input$filter)
  })
  
  filtered4 <- reactive({
    
    batters <- batter_data %>% 
      filter(league %in% input$league_batter)
    
    colnames(batters) <- c("Name", "Age", "Team", "League", "Pitches", "BBE", "Max EV",
                                "95th Pct. EV", "Average EV", "LA Sweet Spot%",
                           "LA Standard Deviation", "OZ-Swing%", "IZ-Contact%", "Swing%" ,
                           "SwStr%", "Whiff%", "Hard Hit%", "Barrel%", "FB%",
                           "FB SwStr%", "BB%", "BB SwStr%", "OFF%", "OFF SwStr%")
    batters <- batters
    
  })
  
  filtered5 <- reactive({
    batters <- batter_data %>% 
      filter(batting_team == input$team2) %>% 
      select(-batting_team, -league)
    
    colnames(batters) <- c("Name", "Age", "Pitches", "BBE", "Max EV",
                           "95th Pct. EV", "Average EV", "LA Sweet Spot%",
                           "LA Standard Deviation",
                           "OZ-Swing%", "IZ-Contact%", "Swing%", "SwStr%", "Whiff%", "Hard Hit%", "Barrel%",
                           "FB%",
                           "FB SwStr%", "BB%", "BB SwStr%", "OFF%", "OFF SwStr%")
    batters <- batters
    
  })
  
  filtered6 <- reactive({
    
    batters <- batter_data %>% 
      filter(matchup.batter.fullName == input$filter2)

    
    colnames(batters) <- c("Name", "Age", "Team", "League", "Pitches", "BBE", "Max EV",
                                "95th Pct. EV", "Average EV", "LA Sweet Spot%",
                           "LA Standard Deviation",
                                "OZ-Swing%", "IZ-Contact%", "Swing%", "SwStr%", "Whiff%", "Hard Hit%", "Barrel%",
                           "FB%",
                           "FB SwStr%", "BB%", "BB SwStr%", "OFF%", "OFF SwStr%")
    
    
    batters <- batters
  })
  

  
  
  output$table <- DT::renderDataTable({
    filtered()
  },     filter = "top",
  extensions = "FixedColumns", 
  rownames = F,
  options = list(
    scrollX = T, 
    scrollY = "500px",
    fixedColumns = list(leftColumns = 1)
  ) )
  
  output$teamtable <- DT::renderDataTable({
    team()
  },     filter = "top",
  extensions = "FixedColumns", 
  rownames = F,
  options = list(
    scrollX = T, 
    scrollY = "500px",
    fixedColumns = list(leftColumns = 1)
  ) )
  
  output$table2 <- renderDataTable({
    filtered2()
  },     filter = "top")
  
  output$table3 <- renderDataTable({
    filtered4()
  },     filter = "top",
  extensions = "FixedColumns", 
  rownames = F,
  options = list(
    scrollX = T, 
    scrollY = "500px",
    fixedColumns = list(leftColumns = 1)
  ) )
  
  output$table4 <- renderDataTable({
    filtered6()
  })
  
  output$teamtable2 <- DT::renderDataTable({
    filtered5()
  },     filter = "top",
  extensions = "FixedColumns", 
  rownames = F,
  options = list(
    scrollX = T, 
    scrollY = "500px",
    fixedColumns = list(leftColumns = 1)
  ) )
  
  # GET RID OF ML_PBP
  #> needs
  #> matchup.batter.fullName
  #> hitData.coordinates.coordX
  #> hitData.coordinates.coordY
  #> hitData.launchSpeed
  #> hitData.launchAngle

  
  
  filtered7 <- reactive({
    batter <- bbe_data %>% 
      filter(matchup.batter.fullName == input$filter2)
    
  })
  
  output$plot2 <- renderPlot({
    spray_chart(filtered7(), aes(x = hitData.coordinates.coordX, 
                                 y = -hitData.coordinates.coordY, 
                                 color = hitData.launchSpeed)) + 
      geom_point(size = 3) +
      scale_color_gradient(low="blue", high="red") +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank()) +
      labs(colour="Exit Velocity") +
      geom_point(data = subset(filtered7(), hitData.launchSpeed > 99.9), shape=17, size=5, na.rm = T)
  })
  
  output$plot99 <- renderPlot({
    ggplot(data = subset(filtered7(), !is.na(hitData.launchAngle)), 
           aes(x = hitData.launchAngle)) +
      geom_density() +
      ggtitle("Launch Angle Density Plot") +
      xlab("Launch Angle") + ylab("Density") +
      geom_vline(xintercept = 8, color = "red") +
      geom_vline(xintercept = 32, color = "red")
  })
  
  output$plot <- renderPlot({
    
    ggplot(data = subset(filtered3(), !is.na(details.type.description)), 
           aes(x = hb, y = ivb, 
               color = details.type.description)) +
      geom_point(size = 3, alpha = .8) +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      ggtitle("Pitch Movement Profile") +
      xlab("HB") + ylab("IVB") +
      guides(color = guide_legend(title = "Pitch"))
  }, height = 350, width = 400)
  
  output$plot3 <- renderPlot({
    ggplot(data = subset(filtered3(), !is.na(details.type.description)),
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
  
  output$plot4 <- renderPlot({
    ggplot(data = subset(filtered8(), !is.na(details.type.description)), 
           aes(x = game_date, y = velo, color = details.type.description, group = details.type.description)) +
      geom_point() +
      geom_line() +
      ggtitle(paste("Velocity by Appearance")) +
      guides(color = guide_legend(title = "Pitch")) +
      ylab('Velocity') +
      xlab("Appearance Date")
  }, height = 350, width = 400)
  
  output$plot5 <- renderPlot({
    ggplot(data = subset(filtered9(), !is.na(details.type.description)), 
           aes(x = game_date, y = pitch_percentage, color = details.type.description, group = details.type.description)) +
      geom_point() +
      geom_line() +
      ggtitle(paste("Pitch Usage by Appearance")) +
      guides(color = guide_legend(title = "Pitch")) +
      ylab('Usage') +
      xlab("Appearance Date")
  }, height = 350, width = 400)
  
  output$plot6 <- renderPlot({
    ggplot(data = subset(filtered8(), !is.na(details.type.description)), 
           aes(x = game_date, y = pfx_z, color = details.type.description, group = details.type.description)) +
      geom_point() +
      geom_line() +
      ggtitle(paste("IVB by Appearance")) +
      guides(color = guide_legend(title = "Pitch")) +
      ylab('IVB') +
      xlab("Appearance Date")
  }, height = 350, width = 400)
  
  output$plot7 <- renderPlot({
    ggplot(data = subset(filtered8(), !is.na(details.type.description)), 
           aes(x = game_date, y = pfx_x, color = details.type.description, group = details.type.description)) +
      geom_point() +
      geom_line() +
      ggtitle(paste("HB by Appearance")) +
      guides(color = guide_legend(title = "Pitch")) +
      ylab('HB') +
      xlab("Appearance Date")
  }, height = 350, width = 400)
  
  
})
