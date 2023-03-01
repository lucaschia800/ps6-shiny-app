library(tidyverse)
library(shiny)
temp_data <- read_delim("UAH-lower-troposphere-long.csv.bz2")

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("ps6 shiny app"),
  p("This is an app designed to relay certain data trends on global temperature changes."),
  tabsetPanel(type="tabs",
              tabPanel("Intro"),
              tabPanel(("Plot 1"),
                       p("this is where the plots will go"),
                       sidebarLayout(
                         sidebarPanel(checkboxGroupInput("regions", label = ("Region(s) to Display"), 
                                                         choices = list("globe_ocean"= "globe_ocean","nh"="nh"),
                                                         selected = "globe_ocean"),
                                      radioButtons("color", label = ("Plot Color"),
                                                   choices = list("Red" = "red", "Blue" = "blue", "Green" = "green"), 
                                                   selected = "red"),
                                      sliderInput("year_range", label = h3("Year Range"), min = 1978, 
                                                  max = 2023, value = c(1978, 2023))),
                         mainPanel(plotOutput("plot"))))

              
  ),
  

)


server <- function(input, output) {

  
  

  output$plot <- renderPlot({
    temp_data_mean <- temp_data %>% 
      group_by(year) %>% 
      summarize(avg_temp=mean(temp)) %>% 
      filter(year>=input$year_range[1],year<=input$year_range[2])
    
    temp_data_rmean <- temp_data %>% 
      group_by(year,region) %>% 
      summarize(avg_temp=mean(temp)) %>% 
      filter(region %in% input$regions)
    
    ggplot()+geom_point(data=temp_data_mean,aes(year,avg_temp),color=input$color)+
      geom_point(data=temp_data_rmean,aes(year,avg_temp,col=region))
      
  }
    
  )

  
}

# Run the application 
shinyApp(ui = ui, server = server)
