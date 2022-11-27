library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(readxl)

CovidDenmark <- read_excel("CovidDenmark.xlsx")
CovidDenmark$date <- as.Date(CovidDenmark$date, format = "%Y-%m-%d")


ui <- fluidPage(
  theme = shinytheme("superhero"),
  
  titlePanel("Corona data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "country",
        label = "Country",
        choices = unique(c(CovidDenmark$location)),
        multiple = TRUE,
        selected = "Denmark"
      ),
    ),
    
    mainPanel(tabsetPanel(
      tabPanel("New_cases/new_vaccinations", plotOutput("map")),
      tabPanel("new_tests/positive_rate", plotOutput("map2"))
      
    ))
  )
)


server <- function(input, output) {
  output$map <- renderPlot({
    ggplot(
      filter(CovidDenmark, location == input$country),
      aes(x = date, y = new_cases, color = location)) + 
      geom_bar(aes(x = date, y = new_vaccinations),
      stat = "identity",
      fill = "pink",
      colour = "pink") +
      geom_line(size = 1) +
      scale_x_date(date_labels = "%m-%Y") +
      xlab("Date") +
      theme_linedraw()
  })
  
  output$map2<-renderPlot({  
    ggplot(filter(CovidDenmark, location==input$country), aes(x=date, y=new_tests_smoothed_per_thousand, color=location)) + geom_bar(aes(x=date, y=positive_rate), stat = "identity", fill="cyan",colour="purple") +
      scale_y_continuous(sec.axis=sec_axis(~.*0.0001,name="Positive Rate")) + 
      geom_line(size = 1) +
      scale_x_date(date_labels = "%m-%Y") +
      xlab("Date") + 
      theme_linedraw()  
  })
}
shinyApp(ui, server)