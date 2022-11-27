library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(readxl)
library(ggborderline)

CovidDenmark <- read_excel("CovidDenmark.xlsx")
covidWorld <- read_excel("owid-covid-data.xlsx")





ui <- fluidPage(theme = shinytheme("superhero"),
                
                titlePanel("Corona data"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "country", label = "Country",
                                choices = unique(c(CovidDenmark$location)), multiple = TRUE, selected = "Denmark")),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Plots", plotOutput("plots")),
                      tabPanel("Animated", plotOutput("aniamted"))
                      
                    )
                  )
                )
)

datebreaks <- seq(as.Date("2020-02-27"), as.Date("2022-10-03"), by = "1 month")


server<-function(input,output){
  output$plots<-renderPlot({
    
    ggplot(filter(CovidDenmark, location==input$country), aes( x = reproduction_rate, y=stringency_index / 13)) +
      geom_histogram(bins= 15, stat="identity",  size = 1)
  })
  
}

    
shinyApp(ui,server)
    