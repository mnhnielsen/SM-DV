library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(readxl)
library(maps)

world_map <- map_data("world")
CovidDenmark <- read_excel("CovidDenmark.xlsx")





ui <- fluidPage(theme = shinytheme("superhero"),
                
  titlePanel("Corona data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "continent", label = "Continent",
                  choices = unique(c(CovidDenmark$continent))),
      selectInput(inputId = "country", label = "Country",
                  choices = unique(c(CovidDenmark$location)))),
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
    ggplot(filter(CovidDenmark, continent==input$continent), aes(x=total_cases,y=total_deaths, color=location)) + geom_point()
    
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
