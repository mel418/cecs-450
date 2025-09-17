rm(list=ls())

library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)

# Sample data - Walking Dead character kills by season
kills <- data.frame(
  Season = 1:6,
  Rick = c(36, 23, 59, 36, 36, 66),
  Morgan = c(3, NA, 2, NA, 5, 33),
  Glen = c(4, 10, 22, 29, 73, 44),
  Daryl = c(8, 19, 41, 68, 43, 41),
  Carl = c(0, 17, 18, 17, 8, 2),
  Michonne = c(NA, 1, 29, 46, 35, 42),
  Carol = c(0, 0, 8, 10, 37, 29),
  Maggie = c(NA, 0, 16, 32, 9, 12)
)

# Convert to long format for ggplot
kills.long <- melt(kills, 
                   id.vars = "Season", 
                   measure.vars = colnames(kills)[2:9],
                   variable.name = "Character", 
                   value.name = "nKills")

# Remove rows with NA values
kills.long <- kills.long[!is.na(kills.long$nKills), ]

# Define UI
ui <- fluidPage(
  titlePanel("Walking Dead Kills"),
  
  sidebarLayout(
    sidebarPanel(
      # Character selection - single character only
      radioButtons("character", 
                  "Character:",
                  choices = unique(kills.long$Character),
                  selected = "Rick"),
      
      # Show bar chart option
      radioButtons("showBarChart",
                  "Show Bar Chart:",
                  choices = list("Yes" = TRUE, "No" = FALSE),
                  selected = TRUE),
      
      # Kill range slider
      sliderInput("killRange",
                 "Kill Range:",
                 min = 0,
                 max = 80,
                 value = c(0, 80))
    ),
    
    mainPanel(
      plotOutput("killsPlot"),
      br(),
      tableOutput("dataTable")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    kills.long %>%
      filter(Character == input$character,
             nKills >= input$killRange[1],
             nKills <= input$killRange[2])
  })
  
  # Main plot
  output$killsPlot <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = "No data to display", 
                     size = 6) +
             theme_void())
    }
    
    if(input$showBarChart) {
      # Bar chart
      ggplot(data, aes(x = Season, y = nKills)) + 
        geom_col(fill = "coral", alpha = 0.8) +
        theme_minimal() +
        labs(title = paste("Kills by", input$character),
             x = "Season",
             y = "Number of Kills") +
        scale_x_continuous(breaks = 1:6)
    } else {
      # Line chart
      ggplot(data, aes(x = Season, y = nKills)) + 
        geom_line(color = "coral", size = 1.2) + 
        geom_point(color = "coral", size = 3) +
        theme_minimal() +
        labs(title = paste("Kills by", input$character),
             x = "Season",
             y = "Number of Kills") +
        scale_x_continuous(breaks = 1:6)
    }
  })
  
  # Data table
  output$dataTable <- renderTable({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(data.frame(Message = "No data matches current filters"))
    }
    
    data %>%
      arrange(Season)
  })
}

# Run the app
shinyApp(ui = ui, server = server)