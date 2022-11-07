library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
library(shinythemes)


owid_covid_data <- read_excel("owid-covid-data.xlsx")
world_map <- map_data("world")


ui <- fluidPage(theme = shinytheme("superhero"),
                
  titlePanel("Corona data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "continent", label = "Continent",
                  choices = unique(c(owid_covid_data$continent))),
      selectInput(inputId = "country", label = "Country",
                  choices = unique(c(owid_covid_data$location)))),
    mainPanel(
      tabsetPanel(
        tabPanel("All", plotOutput("ot")),
        tabPanel("Map", plotOutput("map"))

      )
    )
  )
)

server<-function(input,output){
  output$ot<-renderPlot({
    ggplot(filter(owid_covid_data, continent==input$continent), aes(x=total_cases,y=total_deaths, color=location)) + geom_point()
    
  })
  
  output$map<-renderPlot({
    ggplot(world_map, aes(x = long, y = lat)) +
      geom_polygon(aes( group = group, fill = region))+
      scale_fill_viridis_d()+
      theme_void()+
      theme(legend.position = "none")
    
  })
  
}





shinyApp(ui,server)
