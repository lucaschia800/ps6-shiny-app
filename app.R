library(tidyverse)
library(shiny)
temp_data <- read_delim("UAH-lower-troposphere-long.csv.bz2")

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("ps6 shiny app"),
  p("This is an app designed to relay certain data trends on global temperature changes."),
  tabsetPanel(type="tabs",
              tabPanel("Intro"),
              tabPanel(("Plot 1"),p("this is where the plots will go"))),
  
  



)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
}

# Run the application 
shinyApp(ui = ui, server = server)
