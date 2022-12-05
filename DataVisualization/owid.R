library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(readxl)
library(readr)
library(tidyr)
library(lubridate)
library(ggthemes)
library(gganimate)
library(plotly)
library(gifski)
library(purrr)
library(ggborderline)



CovidDenmark <- read_excel("CovidDenmark.xlsx")
CovidDenmark$date <- as.Date(CovidDenmark$date, format="%Y-%m-%d")

datadk = filter(CovidDenmark, location == "Denmark")
datasw = filter(CovidDenmark, location == "Sweden")
datano = filter(CovidDenmark, location == "Norway")

tddk = max(datadk$total_deaths_per_million, na.rm = TRUE)
tdsw = max(datasw$total_deaths_per_million, na.rm = TRUE)
tdno = max(datano$total_deaths_per_million, na.rm = TRUE)

tcdk = max(datadk$total_cases_per_million, na.rm = TRUE)
tcsw = max(datasw$total_cases_per_million, na.rm = TRUE)
tcno = max(datano$total_cases_per_million, na.rm = TRUE)

tvdk = max(datadk$total_vaccinations_per_hundred, na.rm = TRUE)
tvsw = max(datasw$total_vaccinations_per_hundred, na.rm = TRUE)
tvno = max(datano$total_vaccinations_per_hundred, na.rm = TRUE)

total_vaccinations_per_hundred = c(tvdk, tvsw, tvno)
total_deaths_per_million = c(tddk, tdsw, tdno)
total_cases_per_million = c(tcdk, tcsw, tcno)
locations = unique(c(CovidDenmark$location))

barFrame = data.frame(locations, total_vaccinations_per_hundred, total_deaths_per_million, total_cases_per_million)



ui <- fluidPage(theme = shinytheme("superhero"),

  titlePanel("Corona data"),


sidebarLayout(
  sidebarPanel(
    selectInput(inputId = "country", label = "Country",
                choices = unique(c(CovidDenmark$location)), multiple = TRUE, selected = "Denmark"),
    selectInput(inputId = "outcome", label = "Parameter",
                choices = c("Total Cases per million"=colnames(CovidDenmark)[11], "New Cases per million"=colnames(CovidDenmark)[12], "Total Deaths per million"=colnames(CovidDenmark)[14]))),
    mainPanel(
      tabsetPanel(
        tabPanel("Plots", plotOutput("plots")),
        tabPanel("ICU Patients vs Cases", plotOutput("histo")),
        tabPanel("Stringency vs Pop. Rate", plotOutput("icu")),
        tabPanel("New cases vs vaccinations", plotOutput("newVac")),
        tabPanel("New tests vs positive rate", plotOutput("newTest")),
        tabPanel("New deaths vs Vaccination in Sweden", plotOutput("anim")),
        tabPanel("Animated", imageOutput(outputId = "aniamted", width = "100%")),
        tabPanel("Barchart", plotOutput(outputId = "barplot"), radioButtons(inputId = "var", label = "Choose desired Variable!",
                                                                     choices = c("Total Deaths per million" = "total_deaths_per_million", "Total Vaccinations per hundred" = "total_vaccinations_per_hundred", "Total Cases per million" = "total_cases_per_million"), selected = "total_deaths_per_million"))

      )
    )
  )
)

datebreaks <- seq(as.Date("2020-02-27"), as.Date("2022-10-03"), by = "1 month")

server <- function(input, output) {



  output$aniamted <- renderImage({

    #outfile <- tempfile(fileext='.gif')



    graph =
      ggplot(CovidDenmark, aes(x = as.Date(date), y = total_deaths_per_million, colour = location)) + geom_line(size = 2, alpha = 0.75) +
      theme_solarized_2() +
      labs(title = "Deaths in Scandinavia over time",
           y = "Total Deaths per million",
           x = "Date") +
      theme(text = element_text(family = "DM Sans Medium", colour = "#000000"),
            title = element_text(colour = "#000000"),

            panel.background = element_rect(fill = NA),
            plot.background = element_rect(fill = "#EEEEEE")
           )

    #graph.animation=graph+transition_reveal(as.Date(date))+
    #(fixed_y = TRUE)



    #anim_save("outfile.gif", animate(graph.animation, fps=15,duration=20, end_pause=60, res=100)) 

    list(src = "outfile.gif",
         contentType = 'image/gif',
         width=800,
         height=600
         
    )})#, deleteFile = FALSE)



  data_r <- reactive({
    data = barFrame %>% select(locations, y = input$var)
    return(data)
  })

  output$barplot <- renderPlot({

    data = data_r()

    ggplot(data, aes(x = locations, y = y)) +
      geom_bar(stat = 'identity') +
      ylab(input$var)

  })
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
      geom_line(size = 1) + geom_line(aes(x=date, y=new_deaths_per_million*25), color="blue") +
      scale_y_continuous(sec.axis = sec_axis(~./25, "New Deaths per million")) +
      scale_x_date(date_labels = "%m-%Y") +
      xlab("Date") + 
      ylab("New Vaccinations and New Deaths") +
      theme_classic()
  })

  output$histo<-renderPlot({
    
    ggplot(filter(CovidDenmark, location==input$country), aes(x=date, y=new_cases_per_million, color=location)) + 
      geom_histogram(stat = "identity", size = 1) + 
      geom_line(aes(y=icu_patients_per_million * 100), size=1, bordercolour = "black", color="blue") +
      scale_y_continuous(sec.axis = sec_axis(~./100, "Icu Patients per million")) +
      xlab("Date") + 
      theme_classic() +
      scale_x_date(date_labels = "%m-%Y") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  output$icu<-renderPlot({
    
    ggplot(filter(CovidDenmark, location==input$country), aes( x = reproduction_rate, y=stringency_index / 13)) +
      geom_histogram(bins= 15, stat="identity",  size = 1) + 
      xlab("Reproduction Rate") + 
      ylab("Stringency Indexs")
  })
  output$newVac <- renderPlot({
    ggplot(
      filter(CovidDenmark, location == input$country),
      aes(x = date, y = new_cases_per_million, color = location)) + 
      geom_bar(aes(x = date, y = new_vaccinations_smoothed_per_million),
               stat = "identity",
               fill = "pink",
               colour = "pink") +
      geom_line(size = 1) +
      scale_x_date(date_labels = "%m-%Y") +
      xlab("Date") +
      ylab("New Cases pr million") +
      theme_linedraw()
  })
  
  output$newTest <- renderPlot({
    ggplot(
      filter(CovidDenmark, location == input$country),
      aes(x = date, y = new_tests_smoothed_per_thousand/15, color = location)) + 
      geom_bar(aes(x = date, y = positive_rate),
               stat = "identity",
               fill = "cyan",
               colour = "purple") +
      scale_y_continuous(sec.axis = sec_axis( ~ ./15, name = "Positive Rate")) +
      geom_line(size = 1) +
      scale_x_date(date_labels = "%m-%Y") +
      xlab("Date") +
      ylab("New tests per thousand") +
      theme_linedraw()  
  })
}



shinyApp(ui, server)
