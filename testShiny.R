library(shiny)
ui <- fluidPage(
  titlePanel("Title Element", window = "Window Element"),
  h1("This is a heading"),
  h2("and this is a subheading"),
  "Now this is some text",
  br(),
  "with a line break in it.",
  div("I can also use html type properties like color", 
      style = "color: blue;"),
  "And I can do", strong("bold"), ",", em("italics"), ", etc."
)
server <- function(input, output) {}
shinyApp(ui = ui, server = server)