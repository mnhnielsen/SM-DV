library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)

owid_covid_data <- read_excel("owid-covid-data.xlsx")


ui <- fluidPage(
  titlePanel("Corona data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "continent", label = "Continent",
                  choices = unique(c(owid_covid_data$continent)))),
    mainPanel(
      plotOutput("ot")
      )
    )
  )

server<-function(input,output){
  output$ot<-renderPlot({
    ggplot(filter(owid_covid_data, continent==input$continent), aes(x=total_cases,y=total_deaths, color=location)) + geom_point()
    
  })
}

shinyApp(ui,server)
