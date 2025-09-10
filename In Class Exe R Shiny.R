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
                   measure.vars = colnames(kills)[2:9],  # Fixed column selection
                   variable.name = "Character", 
                   value.name = "nKills")

# Remove rows with NA values
kills.long <- kills.long[!is.na(kills.long$nKills), ]

# Define UI
ui <- fluidPage(
  titlePanel("Walking Dead Character Kills by Season"),
  
  sidebarLayout(
    sidebarPanel(
      # Character selection
      checkboxGroupInput("characters", 
                        "Select Characters:",
                        choices = unique(kills.long$Character),
                        selected = c("Rick", "Daryl", "Glen")),
      
      # Season range
      sliderInput("seasons",
                 "Season Range:",
                 min = 1,
                 max = 6,
                 value = c(1, 6)),
      
      # Plot type selection
      radioButtons("plotType",
                  "Plot Type:",
                  choices = list("Line Plot" = "line",
                               "Bar Chart" = "bar",
                               "Point Plot" = "point"),
                  selected = "line"),
      
      br(),
      
      # Summary statistics checkbox
      checkboxInput("showStats", 
                   "Show Summary Statistics", 
                   value = FALSE)
    ),
    
    mainPanel(
      plotOutput("killsPlot"),
      br(),
      conditionalPanel(
        condition = "input.showStats == true",
        h4("Summary Statistics"),
        tableOutput("statsTable")
      ),
      br(),
      h4("Filtered Data"),
      tableOutput("dataTable")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    kills.long %>%
      filter(Character %in% input$characters,
             Season >= input$seasons[1],
             Season <= input$seasons[2])
  })
  
  # Main plot
  output$killsPlot <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = "No data to display\nSelect characters and seasons", 
                     size = 6) +
             theme_void())
    }
    
    base_plot <- ggplot(data, aes(x = Season, y = nKills, color = Character))
    
    if(input$plotType == "line") {
      base_plot + 
        geom_line(size = 1.2) + 
        geom_point(size = 3) +
        theme_minimal() +
        labs(title = "Character Kills Over Seasons",
             x = "Season",
             y = "Number of Kills",
             color = "Character") +
        scale_x_continuous(breaks = 1:6)
    } else if(input$plotType == "bar") {
      base_plot + 
        geom_col(position = "dodge", alpha = 0.8) +
        theme_minimal() +
        labs(title = "Character Kills by Season",
             x = "Season",
             y = "Number of Kills",
             fill = "Character") +
        scale_x_continuous(breaks = 1:6)
    } else {
      base_plot + 
        geom_point(size = 4, alpha = 0.7) +
        theme_minimal() +
        labs(title = "Character Kills Distribution",
             x = "Season",
             y = "Number of Kills",
             color = "Character") +
        scale_x_continuous(breaks = 1:6)
    }
  })
  
  # Summary statistics table
  output$statsTable <- renderTable({
    data <- filtered_data()
    
    if(nrow(data) == 0) return(NULL)
    
    data %>%
      group_by(Character) %>%
      summarise(
        Total_Kills = sum(nKills, na.rm = TRUE),
        Average_Kills = round(mean(nKills, na.rm = TRUE), 1),
        Max_Season_Kills = max(nKills, na.rm = TRUE),
        Seasons_Active = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(Total_Kills))
  }, striped = TRUE, hover = TRUE)
  
  # Filtered data table
  output$dataTable <- renderTable({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(data.frame(Message = "No data matches current filters"))
    }
    
    data %>%
      arrange(Season, Character)
  }, striped = TRUE)
}

# Run the app
shinyApp(ui = ui, server = server)
