library(gganimate)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(readxl)


CovidDenmark <- read_excel("CovidDenmark.xlsx")
CovidDenmark$date <- as.Date(CovidDenmark$date, format="%Y-%m-%d")
op1<-data.frame(x=c(01-2021),y=c(5000))

g<-ggplot(filter(CovidDenmark, location=="Sweden"), aes(x=date,y=new_vaccinations_smoothed_per_million, color=location)) + 
  geom_line(size = 1) + geom_line(aes(x=date, y=new_deaths)) +
  scale_x_date(date_labels = "%m-%Y") +
  xlab("Date") + 
  ylab("New Vaccinations and New Deaths") +
  theme_classic()

g
