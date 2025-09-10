library(shiny)
library(ggplot2)
library(dplyr)

# Load the BC Liquor data
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

# Clean the data - remove rows with missing values for key variables
bcl <- bcl %>%
  filter(!is.na(Price), !is.na(Alcohol_Content), !is.na(Type), !is.na(Country))

# Define UI
ui <- fluidPage(
  titlePanel("BC Liquor Store Prices", windowTitle = "CECS 614 Lecture 7"),
  
  sidebarLayout(
    sidebarPanel(
      # Price slider
      sliderInput("priceInput", 
                  "Price", 
                  min = 0, 
                  max = ceiling(max(bcl$Price, na.rm = TRUE)), 
                  value = c(10, 20), 
                  pre = "$"),
      
      # Product type radio buttons
      radioButtons("typeInput", 
                   "Product type", 
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      
      # Country selection (dynamic)
      uiOutput("countryOutput")
    ),
    
    mainPanel(
      plotOutput("coolplot"),
      br(),
      textOutput("nrow"),
      br(),
      tableOutput("results")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Dynamic country selection
  output$countryOutput <- renderUI({
    selectInput("countryInput", 
                "Country",
                choices = sort(unique(bcl$Country)), 
                selected = "CANADA")
  })
  
  # Reactive filtered data
  filtered <- reactive({
    if(is.null(input$countryInput)) return(NULL)
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput)
  })
  
  # Histogram plot
  output$coolplot <- renderPlot({
    if(is.null(filtered())) {
      return()
    }
    
    data <- filtered()
    
    if(nrow(data) == 0) {
      return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                     label = "No data matches your criteria", 
                     size = 6) +
             theme_void())
    }
    
    ggplot(data, aes(Alcohol_Content)) + 
      geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Distribution of Alcohol Content",
           x = "Alcohol Content (%)",
           y = "Count") +
      theme(plot.title = element_text(size = 16, hjust = 0.5))
  })
  
  # Results table
  output$results <- renderTable({
    if(is.null(filtered())) return(NULL)
    
    data <- filtered()
    
    if(nrow(data) == 0) {
      return(data.frame(Message = "No results found for your criteria"))
    }
    
    # Show a sample of results (first 10 rows)
    data %>%
      select(Name, Type, Country, Alcohol_Content, Price, Sweetness) %>%
      head(10)
  })
  
  # Number of results text
  output$nrow <- renderText({
    if(is.null(filtered())) return("")
    
    nn <- nrow(filtered())
    paste("Based on your criteria, there were", nn, "results found.")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)