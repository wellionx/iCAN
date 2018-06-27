library(ggplot2)
library(recharts)
library(plyr)
library(data.table)

setwd("D:\\Data\\datav")
load(file = "Weather of 30 years clean data to2017.rda")#pick up the station we need 
# two stations in Xinjiang
# DT <- DT30ys[stationID %in% c("51133")]  # tacheng
# 
# DT <- DT30ys[stationID %in% c("51076")]  # alatai

DT <- DT30ys[stationID %in% c("54254")]  # kaiyuan

DT <- DT30ys[stationID %in% c("54260")]  # xifeng
DT <- DT30ys[stationID %in% c("53446")]  # Neimeng 巴彦淖尔

wth <- DT

wth$Date <- as.Date(paste(wth$Year, wth$Month, wth$Day, sep='-'))

head(wth)
#wth for vis with GST
# wth2 <- ddply(wth, .(Date, stationID), summarise,
#               
#               Year = Year,
#               solarhr = round(SSD/10,1),
#               Tmax = round(TEM_H/10,1),
#               Tmin = round(TEM_L/10,1),
#               humidity = RHU_M,
#               rainfall = round(PRE_C/10,1),
#               GST =  round(GST_A/10,1))
#more fast with data.table
wth2 <- wth[,list(Year = Year,
                  solarhr = round(SSD/10,1),
                  Tmax = round(TEM_H/10,1),
                  Tmin = round(TEM_L/10,1),
                  humidity = RHU_M,
                  rainfall = round(PRE_C/10,1),
                  GST =  round(GST_A/10,1)),
            by=.(stationID,Date)]
# get the Day of Year
doyFromDate <- function(date) {
  date <- as.character(date)
  as.numeric(format(as.Date(date), "%j"))
}
wth2$DOY <- doyFromDate(wth2$Date)
head(wth2)
# drop the stationID
wth2$stationID <- NULL
#check missing data ####
library(mice)
im <- wth2
md.pattern(im) 
anyNA(im)
#KNN插值
library(DMwR)
im <- wth2[,-1]
knnOutput <- knnImputation(im)  # 使用KNN插值.
anyNA(knnOutput)
wth2_im <- cbind(wth2[,1],knnOutput)

#files for weather vis
# wth_tch2017s <- wth2_im
# wth_alt2017s <- wth2_im
# wth_kaiy2017s <- wth2
# save(wth_tch2017s, wth_alt2017s, file="wth file of Xinjiang20180409 for vis.rda")
# save(wth_kaiy2017s, file="wth file of kaiyuan for vis.rda")

# wth <- wth_chx
# rm(list = ls())
# load(file = "wth file of Xinjiang20180409 for vis.rda")
# load(file="wth file of kaiyuan for vis.rda")
# wth <- wth_tch2017s
# wth <- wth_alt2017s

wth <- wth2
head(wth)
wth$Year <- factor(wth$Year)

dayFromDate <- function(date) {
  date <- as.character(date)
  as.numeric(format(as.Date(date), "%d"))
}
monthFromDate <- function(date) {
  date <- as.character(date)
  as.numeric(format(as.Date(date), "%m"))
}
wth$Month <- monthFromDate(wth$Date)
wth$Month <- factor(wth$Month)
wth$Day <- dayFromDate(wth$Date)
wth$Day <- factor(wth$Day)

# Rainfall yearly
require(dplyr)
library(recharts)
wth.yearly <- wth %>%
  group_by(Year) %>%
  summarise(Rainf = sum(rainfall))

echartr(wth.yearly, Year, Rainf,type='vbar') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='年份')%>% 
  setYAxis(name='降雨量', splitNumber=5, axisLabel=list(
    formatter='%.0f mm'))%>% 
  setTitle('巴彦淖尔市近30年年度降雨量')

#Rainfall monthly
wth.monthly <- wth %>%
  group_by(Year,Month) %>%
  summarise(Rainfall = sum(rainfall))
# 带时间轴的条图Bar Chart with Timeline
# options(repr.plot.width=4, repr.plot.height=3)
echartr(wth.monthly, Month, Rainfall, t=Year, type='vbar') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='月份')%>% 
  setYAxis(name='降雨量', splitNumber=5, axisLabel=list(
    formatter='%.0f mm'))%>% 
  setTitle("Rainfall in each month / mm")

#May-Aug rainfall
crtRain <- filter(wth.monthly,Month %in% c("5","6","7","8"))
#取子集后factor数量没有变化，需要对取子集的变量重新factor
crtRain$Month <- factor(crtRain$Month)
echartr(crtRain, Year, Rainfall, Month,type='column', subtype='stack') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='年份')%>% 
  setYAxis(name='降雨量', splitNumber=5, axisLabel=list(
    formatter='%.0f mm'))%>% 
  setTitle('巴彦淖尔市近30年5-8月份累积降雨量')

#播种决策，气温和降水
# temperature
# tp <-filter(wth,Month %in% c("4","5","6","7","8","9"))
# 
# tp$Month <- factor(tp$Month, labels=c("Apr","May", "Jun", "Jul", "Aug", "Sep"))
# # Combine ground temp with rainfall
# gdrn <- filter(tp,Year %in% "2016")
# library(reshape)
# gdrn1 <- melt(gdrn, id=c('Month', 'Day'), measure=c('GST', 'rainfall'), 
#               variable='Param')
# g4 <- echartr(gdrn1, Day, value, Param, t=Month, type='curve')
# 
# g4 %>% setY1Axis(series='rainfall', axisLine=list(lineStyle=lineStyle(width=0)), 
#                  name='Rainfall', axisLabel=list(formatter="%.1f mm"), min=0) %>%
#   setXAxis(splitLine=list(show=FALSE), axisLine=list(
#     lineStyle=lineStyle(color='darkgray'))) %>% 
#   setYAxis(axisLine=list(lineStyle=lineStyle(width=0)), 
#            axisLabel=list(formatter="%.1f °C"), min=0, name='GroundTemp') %>% 
#   setGrid(borderWidth=0) %>%
#   setTitle('Ground Temperature and Rainfall in 2017') 

#look at the two month of April and May
tp <-filter(wth,Month %in% c("4","5"))
# Combine ground temp with rainfall
gdrn <- filter(tp,Year %in% "2016")
library(reshape)
gdrn1 <- melt(gdrn, id=c('Month', 'Day'), measure=c('GST', 'rainfall'), 
              variable='Param')
gdrn1$monthday <- paste(gdrn1$Month, gdrn1$Day, sep = "-")
g4 <- echartr(gdrn1, monthday, value, Param, type='curve')

g4 %>% setY1Axis(series='rainfall', axisLine=list(lineStyle=lineStyle(width=0)), 
                 name='降水量', axisLabel=list(formatter="%.1f mm"), min=0,max=30) %>%
  setXAxis(name="日期",splitLine=list(show=FALSE), axisLine=list(
    lineStyle=lineStyle(color='darkgray'))) %>% 
  setYAxis(axisLine=list(lineStyle=lineStyle(width=0)), 
           axisLabel=list(formatter="%.1f °C"), min=0,max=35, name='地表温度') %>% 
  setGrid(borderWidth=0) %>% 
  setToolbox(pos=3) %>%
  setTitle('Ground Temperature and Rainfall in history') 

#look at the two month of April and May in each year
tp <-filter(wth,Month %in% c("4","5"))
library(reshape)
gdrn <- melt(tp, id=c('Year','Month', 'Day'), measure=c('GST', 'rainfall'), 
              variable='Param')
gdrn$monthday <- paste(gdrn$Month, gdrn$Day, sep = "-")
#order the variable
gdrn <- gdrn[with(gdrn, order(Year,Month,Day)), ]

g4 <- echartr(gdrn, monthday, value, Param,t=Year, type='curve')

g4 %>% setY1Axis(series='rainfall', axisLine=list(lineStyle=lineStyle(width=0)), 
                 name='降水量', axisLabel=list(formatter="%.1f mm"), min=0,max=30) %>%
  setXAxis(name="日期",splitLine=list(show=FALSE), axisLine=list(
    lineStyle=lineStyle(color='darkgray'))) %>% 
  setYAxis(axisLine=list(lineStyle=lineStyle(width=0)), 
           axisLabel=list(formatter="%.1f °C"), min=0,max=35, name='地表温度') %>% 
  setGrid(borderWidth=0) %>% 
  setToolbox(pos=3) %>%
  setTitle('Ground Temperature and Rainfall in history') 
