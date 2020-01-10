library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library("readxl")
library(dplyr)
detach(package:plyr)
require(xts)
library(chron)
library(lubridate)

I656 <- read_csv('data/I-65-Here.csv')
I653 <- read_csv('data/I-65.csv')   
tmc <- read_csv('data/TMC_Identification.csv')

#drop column to match columns from I656
I653 <- I653[,1:6]

#combined 2 tables for years 2011-2018
final_I65 <- rbind(I656, I653)



#drop NA's, add date, time, year, month, and day columns
final_I65 <- na.omit(final_I65)
final_I65$Date <- as.Date(final_I65$measurement_tstamp)
final_I65$Time <- format(as.POSIXct(strptime(final_I65$measurement_tstamp)), format = "%H:%M:%S")
final_I65$Year <- as.integer(format(final_I65$Date,'%Y'))
final_I65$Month <- as.integer(format(final_I65$Date,'%m'))
final_I65$Day <- as.integer(format(final_I65$Date,'%d'))

format(as.POSIXct(strptime(weather$Time,"%d/%m/%Y %H:%M",tz="")) ,format = "%H:%M")


#add days of the week column
final_I65$DoW <- weekdays(final_I65$Date, abbreviate = F)

#change 1st column name to tmc_code in tmc file2
colnames(tmc)[1]<-"tmc_code"

#drop unnecessary columns from tmc file
tmc <- subset(tmc, select = -c(d, b))

#join geography info
final_I65 <- merge(final_I65,tmc,by="tmc_code")

#some exploratory analysis
final_I65 %>% 
  select(speed) %>% 
  summary()
#minimum speed is 1 mph, max is 99 which shows a very large range.
#at what time are peeople going nearly 100 mph
#the mean is 52.85 and the median 56. It could be there are many outliers 
#at the lower end of the of the dataset


final_I65 %>% 
  filter(Year %in% c(2011, 2012)) %>% 
  select(speed) %>% 
  summary()

final_I65 %>% 
  filter(Year %in% c(2017, 2018)) %>% 
  select(speed) %>% 
  summary()


#What days were the speeds lower than 10 mph?
final_I65 %>% 
  filter(Year %in% c(2011, 2012), speed < 10) %>% 
  select(speed, Time, Month, Year, Day) 
#Nov 2-4 around 4 on 2nd, 9:30-10 on the 3rd, 9:45 and 2 on 4th

#Scatter plot for ave speed
I653_by_Wed <- final_I65 %>%
  filter(DoW=="Wednesday")%>%
  group_by(Year, Month) %>% 
  summarise(Ave_Speed = mean(speed))

ggplot(I653_by_Wed, aes(x = Month, y = Ave_Speed)) +
  geom_point(stat="identity") + scale_x_continuous(
    breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))

#Line plot by month for 2012 for speeds

I65_2012 <- final_I65 %>%
  filter(Year==2012)%>%
  group_by(Month) %>% 
  summarise(Ave_Speed = mean(speed))


ggplot(I65_2012, aes(x = Ave_Speed, y = Month)) +
  geom_line() + expand_limits(x=52) + scale_y_continuous(
    breaks=c(12,11,10,9,8,7,6,5,4,3,2,1))

#facet plot over the years
I65_over_yrs <- final_I65 %>%
  group_by(Month, Year) %>% 
  summarise(Avg_Speed = mean(speed))

ggplot(I65_over_yrs, aes(x=Month, y= Avg_Speed)) + geom_point() + scale_x_log10() + facet_wrap(~Year)+ scale_x_continuous(
  breaks=c(12,11,10,9,8,7,6,5,4,3,2,1))

#line plot over the years
ggplot(I65_over_yrs, aes(x=Month, y=Avg_Speed, group = Year, color = as.factor(Year))) + geom_line()+ scale_x_continuous(
  breaks=c(12,11,10,9,8,7,6,5,4,3,2,1))



#Comparing congestion to time of day

rush_hr1<- final_I65 %>%
  filter(Time >= ('06:00:00') & Time <= ('09:00:00')) %>% 
  group_by(Month, Time, Year) %>% 
  summarise(Ave_Speed = mean(speed))
  
rush_hr2<- final_I65 %>%
  filter(Time >= ('16:00:00') & Time <= ('18:30:00')) %>% 
  group_by(Month, Time, Year) %>% 
  summarise(Ave_Speed = mean(speed)) 

#Combining Morning Rush hour with Evening Rush Hour 
rush_hr<- rbind(rush_hr1, rush_hr2)

ggplot(rush_hr, aes(x = Time, y = Ave_Speed)) +
  geom_col()



#Pulling out 2011 data w/timestamp
I65_2011<- final_I65 %>%
  filter(Year==2011) %>% 
  group_by(Month, Year, measurement_tstamp) %>% 
  summarise(Ave_Speed = mean(speed))


#2011 data average by 30 min intervals
I65_2011$TimeFloor <- floor_date(I65_2011$measurement_tstamp, "30 mins")
I65_2011 <- I65_2011 %>% 
  group_by(TimeFloor, Month, Year) %>% 
  summarize(mean(Ave_Speed))

#Split out timestamp again
I65_2011$Date <- as.Date(I65_2011$TimeFloor)
I65_2011$Time <- format(as.POSIXct(I65_2011$TimeFloor), format = "%H:%M:%S")
I65_2011$Day <- as.integer(format(I65_2011$Date,'%d'))
colnames(I65_2011)[4]<-"Avg Speed"



I65_2011

saveRDS(I65_2011, file= "/data/I65_2011.RDS")


final_I65$TimeFloor <- floor_date(final_I65$measurement_tstamp, "30 mins")
final_I65 <- final_I65 %>% 
  group_by(TimeFloor) #%>% 
  #summarize(Avg_Speed = mean(speed))

final_I65$Date <- as.Date(final_I65$TimeFloor)
final_I65$Time <- format(as.POSIXct(final_I65$TimeFloor), format = "%H:%M:%S")
final_I65$Year <- as.integer(format(final_I65$Date,'%Y'))
final_I65$Month <- as.integer(format(final_I65$Date,'%m'))
final_I65$Day <- as.integer(format(final_I65$Date,'%d'))

saveRDS(I65_over_yrs, file= "data/I65_OTY.RDS")
