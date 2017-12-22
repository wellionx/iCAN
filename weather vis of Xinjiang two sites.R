setwd("D:/data/datav")
load(file = "wth of two sites in Xinjiang.rda")
library(ggplot2)
library(recharts)

# wth <- wth_chx
wth <- wth_tch
head(wth)
wth$Year <- factor(wth$Year)
wth$Month <- factor(wth$Month)
wth$Day <- factor(wth$Day)

# Rainfall yearly
require(dplyr)
wth.yearly <- wth %>%
  group_by(Year) %>%
  summarise(Rainf = sum(rainfall))
echartr(wth.yearly, Year, Rainf,type='vbar') %>%
  setTitle('Rainfall across years') 

#Rainfall monthly
wth.monthly <- wth %>%
  group_by(Year,Month) %>%
  summarise(Rainfall = sum(rainfall))
# 带时间轴的条图Bar Chart with Timeline
# options(repr.plot.width=4, repr.plot.height=3)
echartr(wth.monthly, Month, Rainfall, t=Year, type='vbar') %>% 
  setTitle("Rainfall in each month / mm")
#Rainfall daily
rf <-filter(wth,Month %in% c("4","5","6","7","8","9")) 
rf$Month <- factor(rf$Month, labels=c("Apr","May", "Jun", "Jul", "Aug", "Sep"))
echartr(rf, Day, rainfall, Month, t= Year,type='line') %>%
  setTitle('Rainfall daily May - Sep, by Month') %>% 
  setSymbols('emptycircle')
# temperature
tp <-filter(wth,Month %in% c("4","5","6","7","8","9")) 
tp$Month <- factor(tp$Month, labels=c("Apr","May", "Jun", "Jul", "Aug", "Sep"))
echartr(tp, Day, TEM_M, Month, t= Year,type='line') %>%
  setTitle('Temperature May - Sep, by Month') %>% 
  setSymbols('emptycircle')

# Year of 2016 with different temp type ####
tp2 <- filter(tp,Year %in% "2016")
tp2 <- tp2[,2:7]
library(reshape2)
tp2l <- melt(tp2, id.var = c("Year","Month","Day"))
names(tp2l)[5] <- "Tempreture"
tp2l$variable <- factor(tp2l$variable, labels=c("Media", "Highest", "Lowest"))
echartr(tp2l, Day, Tempreture, variable,t=Month, type='line') %>%
  setTitle('Temperature May - Sep 2016, by Month') %>% 
  setSymbols('emptycircle')
  # subset(tp2l, Month== "Jun")

# Ground temp
echartr(tp, Day, GST, Month, t= Year,type='line') %>%
    setTitle('Chaxian Temperature May - Sep, by Month') %>% 
    setSymbols('emptycircle')

# Combine ground temp with rainfall
gdrn <- filter(tp,Year %in% "2016")
  
gdrn1 <- melt(gdrn, id=c('Month', 'Day'), measure=c('GST', 'rainfall'), 
            variable='Param')
g4 <- echartr(gdrn1, Day, value, Param, t=Month, type='curve')

g4 %>% setY1Axis(series='rainfall', axisLine=list(lineStyle=lineStyle(width=0)), 
                 name='Rainfall', axisLabel=list(formatter="%.1f mm"), min=0) %>%
  setXAxis(splitLine=list(show=FALSE), axisLine=list(
    lineStyle=lineStyle(color='darkgray'))) %>% 
  setYAxis(axisLine=list(lineStyle=lineStyle(width=0)), 
           axisLabel=list(formatter="%.1f °C"), min=0, name='GroundTemp') %>% 
  setGrid(borderWidth=0) %>%
  setTitle('Ground Temperature and Rainfall in 2017') 
  


