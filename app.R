library(tidyverse)
library(shiny)
temp_data <- read_delim("UAH-lower-troposphere-long.csv.bz2")

row_info <- nrow(temp_data)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Global Temperature Data App"),
  tabsetPanel(type="tabs",
              tabPanel("Intro",
                       p("This is an app designed to relay certain data trends on global temperature changes. The ", em("temp"), "value is
                         measured in ", strong("Celsius.")),
                       p("The data used for this app is available",a("here",href="https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")),
                       p("There are ",row_info, "rows of data within the set"),
                       p("Below is a table of randomly selected rows to show how the data is structured:"),
                       mainPanel(tableOutput("sample_data")),
              ),
              tabPanel(("Plot"),
                       
                       sidebarLayout(
                         sidebarPanel(checkboxGroupInput("regions", label = ("Region(s) to Display"), 
                                                         choices = list("globe_ocean"= "globe_ocean","nh"="nh","globe"="globe",
                                                                        "sopol_ocean"="sopol_ocean","trpcs"="trpcs","aust"="aust", "usa48"="usa48",
                                                                        "sh"="sh","sh_land"="sh_land","soext"="soext", "soext_land"="soext_land",
                                                                        "soext_ocean"="soext_ocean","sopol"="sopol", "trpcs_land"="trpcs_land",
                                                                        "usa49"="usa49","noext"="noext","noext_land"="noext_land", 
                                                                        "noext_ocean"="noext_ocean","nopol"="nopol","nopol_land"="nopol_land",
                                                                        "nopol_ocean"="nopol_ocean","sopol_land"="sopol_land"),
                                                         selected = "globe_ocean"),
                                      radioButtons("color", label = ("Plot Color"),
                                                   choices = list("Palette 1" = "Spectral", "Palette 2" = "PRGn"), 
                                                   selected = "Spectral"),
                                      sliderInput("year_range", label = h3("Year Range"), min = 1978, 
                                                  max = 2023, value = c(1978, 2023))),
                         mainPanel(plotOutput("plot"),  textOutput("data_info"))),
                       
                       
              ),
              tabPanel(("Data Table"),
                       sidebarPanel(
                         radioButtons("order", label=("Order of Avg Yearly Temp"), choices = list("Descending"= "desc", "Ascending"="asc"),
                                      selected="desc"),
                         sliderInput("year_range_data", label = h3("Year Range"), min = 1978, 
                                     max = 2023, value = c(1978, 2023)),
                         textOutput("data_info_2")),
                       mainPanel(tableOutput("data_table"))
              )
              
              
              
  ),
  
  
)


server <- function(input, output) {
  
  
  
  
  output$plot <- renderPlot({
    
    
    temp_data_rmean <- temp_data %>% 
      group_by(year,region) %>% 
      summarize(avg_temp=mean(temp)) %>% 
      filter(region %in% input$regions,year>=input$year_range[1],year<=input$year_range[2])
    
    ggplot()+geom_point(data=temp_data_rmean,aes(year,avg_temp,col=region))+labs(x="year",y="average temp",title="Average Regional Temperature by Year")+
      scale_color_brewer(palette = input$color)
    
    
  })
  
  output$sample_data <- renderTable({
    temp_data %>% 
      sample_n(5)
  })
  
  output$data_info <- renderText({
    temp_data_info<- temp_data %>% 
      group_by(region) %>% 
      summarize(avg_temp=mean(temp)) %>%
      filter(region %in% input$regions) %>% 
      filter(rank(desc(avg_temp))==1)
    
    paste("Of your selected region(s)",temp_data_info$region, "has the highest average temperature between 1978-2023 at:", temp_data_info$avg_temp)
    
    
    
  })
  
  output$data_table <- renderTable({
    
    
    
    temp_data_year <- temp_data %>% 
      group_by(year) %>% 
      summarize(avg_temp=mean(temp)) %>% 
      filter(year>=input$year_range_data[1],year<=input$year_range_data[2])
    
    if(input$order=="desc") {
      temp_data_year %>% 
        arrange(desc(avg_temp))
      
    }
    else{
      temp_data_year %>% 
        arrange(avg_temp)
      
    }
    
    
  })
  
  output$data_info_2 <- renderText({
    
    temp_data_year <- temp_data %>% 
      group_by(year) %>% 
      summarize(avg_temp=mean(temp)) %>% 
      filter(year>=input$year_range_data[1],year<=input$year_range_data[2]) %>% 
      summarize(avg_temp_period=round(mean(avg_temp),digits=2)) %>% 
      pull(avg_temp_period) %>% 
      paste("The average temperature globally for your selected period is:", .)
    
  })
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
