library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library(censusapi)
library("readxl")




I656 <- read_csv('data/I-65-Here.csv')
I653 <- read_csv('data/I-65-wnull.csv')         


I653



I65$Date <- as.Date(I65$measurement_tstamp)
I65$Time <- format(as.POSIXct(I65$measurement_tstamp) ,format = "%H:%M:%S")
I65

data$date <- as.character(as.Date(data$Date))


I653 <- na.omit(I653)
I653$Date <- as.Date(I653$measurement_tstamp)
I653$Time <- format(as.POSIXct(I653$measurement_tstamp) ,format = "%H:%M:%S")
I653$Year <- as.numeric(format(I653$Date,'%Y'))
I653$Month <- as.numeric(format(I653$Date,'%m'))

#Line graph ave speed
I653_by_YR <- I653 %>%
  group_by(Year)%>%
  summarise(Ave_Speed = mean(speed))

ggplot(I653_by_YR, aes(x = Year, y = Ave_Speed)) +
  geom_line()

#Comparing congestion to time of day





I653_bar <- I653

