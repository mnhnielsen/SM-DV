library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(readxl)

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
        tabPanel("Plots", plotOutput("plots")),
        tabPanel("Animated", plotOutput("aniamted"))

      )
    )
  )
)

server<-function(input,output){
  output$plots<-renderPlot({
    ggplot(filter(CovidDenmark, continent==input$continent), aes(x=total_cases,y=total_deaths, color=location)) + geom_point()
    
  })
  
  output$aniamted<-renderPlot({
    ggplot(filter(CovidDenmark, continent==input$continent), aes(x=total_cases,y=total_deaths, color=location)) + geom_point()
    
  })
}





shinyApp(ui,server)
