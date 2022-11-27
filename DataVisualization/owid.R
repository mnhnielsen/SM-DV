library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(readxl)
library(plotly)
library(gganimate)

CovidDenmark <- read_excel("CovidDenmark.xlsx")
CovidDenmark$date <- as.Date(CovidDenmark$date, format="%Y-%m-%d")






ui <- fluidPage(theme = shinytheme("superhero"),
                
  titlePanel("Corona data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "country", label = "Country",
                  choices = unique(c(CovidDenmark$location)), multiple = TRUE, selected = "Denmark"),
      selectInput(inputId = "outcome", label = "Parameter",
                  choices = c("Total Cases"=colnames(CovidDenmark)[5], "New Cases"=colnames(CovidDenmark)[6], "Total Deaths"=colnames(CovidDenmark)[8]))),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plots", plotOutput("plots")),
        tabPanel("New deaths vs Vaccination in Sweden", plotOutput("anim"))

      )
    )
  )
)


server<-function(input,output){
  output$plots<-renderPlot({  
    ggplot(filter(CovidDenmark, location==input$country), aes(x=date,y=!!as.symbol(input$outcome), color=location)) + 
      geom_line(size = 1) + 
      scale_x_date(date_labels = "%m-%Y") +
      xlab("Date") + 
      ylab(input$outcome) +
      theme_classic()
  })
  output$anim<-renderPlot({  
    ggplot(filter(CovidDenmark, location=="Sweden"), aes(x=date,y=new_vaccinations_smoothed_per_million, color=location)) + 
      geom_line(size = 1) + geom_line(aes(x=date, y=new_deaths)) +
      scale_x_date(date_labels = "%m-%Y") +
      xlab("Date") + 
      ylab("New Vaccinations and New Deaths") +
      theme_classic()
  })
}





shinyApp(ui,server)
