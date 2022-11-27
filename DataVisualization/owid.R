library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(readxl)
library(readr)
library (tidyr)
library(lubridate)
library(ggthemes)
library(gganimate)
library(gifski)
library(plotly)
library(purrr)


CovidDenmark <- read_excel("CovidDenmark.xlsx")

datadk= filter(CovidDenmark, location == "Denmark")
datasw = filter (CovidDenmark , location == "Sweden")
datano = filter (CovidDenmark , location == "Norway")

tddk= max(datadk$total_deaths, na.rm=TRUE)
tdsw= max(datasw$total_deaths, na.rm=TRUE)
tdno = max(datano$total_deaths, na.rm=TRUE)

tcdk =max(datadk$total_cases, na.rm=TRUE)
tcsw =max(datasw$total_cases, na.rm=TRUE)
tcno =max(datano$total_cases, na.rm=TRUE)

tvdk = max(datadk$total_vaccinations, na.rm=TRUE)
tvsw = max(datasw$total_vaccinations, na.rm=TRUE)
tvno = max(datano$total_vaccinations, na.rm=TRUE)

total_vaccinations = c(tvdk,tvsw,tvno)
total_deaths = c(tddk,tdsw,tdno)
total_cases = c(tcdk,tcsw,tcno)
locations = unique(c(CovidDenmark$location))

barFrame = data.frame(locations,total_vaccinations, total_deaths,total_cases)



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
        tabPanel("Animated", imageOutput(outputId = "aniamted",width="100%")),
        tabPanel("Lasse-Barchart",plotOutput(outputId= "barplot"),radioButtons(inputId="var",label="Choose desired Variable!",
                                                                     choices=c("Total Deaths"="total_deaths","Total Vaccinations"="total_vaccinations","Total Cases"="total_cases"),selected ="total_deaths"))

      )
    )
  )
)

server <- function(input, output) {
  
  

  output$aniamted <- renderImage({
    
    outfile <- tempfile(fileext='.gif')
  
    
    
    graph=   
      ggplot(CovidDenmark,aes(x=as.Date(date),y=total_deaths,colour=location))+geom_line(size=2, alpha=0.75)+
      theme_solarized_2()+
      labs(title="Deaths vs Total cases in Scandinavia",
           y="Total Deaths",
           x="Date")+
      theme(text=element_text(family="DM Sans Medium",colour="#000000"),
            title=element_text(colour="#000000"),
            
            panel.background = element_rect(fill=NA),
            plot.background = element_rect(fill="#EEEEEE")
           )
    
    graph.animation=graph+transition_reveal(as.Date(date))+
      view_follow(fixed_y = TRUE)
    
   

    anim_save("outfile.gif", animate(graph.animation, fps=15,duration=20, end_pause=60, res=100)) 
   
    list(src = "outfile.gif",
         contentType = 'image/gif',
         width=800,
         height=600
        
    )}, deleteFile = TRUE)
  
  
  
  data_r<-reactive({
    data = barFrame %>% select(locations, y = input$var)
    return(data)
  })
  
  output$barplot <- renderPlot({
    
    data = data_r()
    
    ggplot(data,aes(x=locations, y=y))+
      geom_bar(stat='identity')+
      ylab(input$var)
    
  })
  
  
  }



shinyApp(ui,server)
