load("D:\\Data\\datav\\Weather of 30 years clean data to2017.rda")
#pick up the station we need 
library(data.table)
library(dplyr)

#吉林地块
DT <- DT30ys[stationID %in% c("54063")]  # 吉林金珠乡

#塔城地块
DT <- DT30ys[stationID %in% c("51133")]  # tacheng
load(file="daily weather of year 2018.rda")
#load("C:/Users/Wei Li/Desktop/历史相似年份/daily weather of year 2018.rda")
DT18 <- wth.dly[Station_Id_C %in% c("51133")]
#历史相似年份为2013年

head(DT)

wth <- DT
adrnm <- "塔城"

wth$Date <- as.Date(paste(wth$Year, wth$Month, wth$Day, sep='-'))
wth$Date <- factor(wth$Date)
wth$Year <- factor(wth$Year)
wth$Month <- factor(wth$Month)
wth$Day <- factor(wth$Day)
wth$monthday <- paste(wth$Month, wth$Day, sep = "-")
#秋风分析 ####
windy <- wth[,c("Date", "Year", "Month","Day","monthday","WIN_A")]
windy

library(recharts)
setkey(windy, Date)
setDF(windy)
#数据按照日期进行排序
windy$WIN <- round(windy$WIN_A/10,1)

DailyWD <- filter(windy, Year==2013)
#全年风速，分析“秋风大” ####
echartr(DailyWD, monthday, WIN, type="curve")%>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', splitNumber=7, axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='风速', splitNumber=8, axisLabel=list(
    formatter='%.0f m/s'))%>% 
  #setTitle('塔城地区近30年6-7月份风速变化')
  setTitle(paste(adrnm,'地块风速变化',sep=''))

#播种决策，地温和降水 ####
#look at the two month of April and May
grdtp_rf <- wth[,c("Date", "Year", "Month","Day","TEM_M","GST_C","PRE_C")]
dt <- filter(grdtp_rf,Month %in% c("4","5"))
dt$GST <- dt$GST_C/10
dt$TEM <- dt$TEM_M/10
dt$rainfall <- dt$PRE_C/10
# Combine ground temp with rainfall
gdrn <- filter(dt,Year %in% "2013")

#用中文变量名
gdrn$平均气温 <- gdrn$TEM
gdrn$降水量 <- gdrn$rainfall

library(reshape)
gdrn2 <- melt(gdrn, id=c('Month', 'Day'), measure=c('平均气温', '降水量'), 
              variable='Param')

gdrn2$monthday <- paste(gdrn2$Month, gdrn2$Day, sep = "-")

g2 <- echartr(gdrn2, monthday, value, Param, type='curve')

g2 %>% setY1Axis(series='降水量', axisLine=list(lineStyle=lineStyle(width=0)), 
                 name='降水量', axisLabel=list(formatter="%.1f mm"), min=0,max=30) %>%
  setXAxis(name="日期",splitLine=list(show=FALSE), axisLine=list(
    lineStyle=lineStyle(color='darkgray'))) %>% 
  setYAxis(axisLine=list(lineStyle=lineStyle(width=0)), 
           axisLabel=list(formatter="%.1f °C"), name='平均气温') %>% 
  setGrid(borderWidth=0) %>% 
  setToolbox(pos=3) %>%
  setLegend(pos = 12) %>%
  setTitle('玉米播种期前后平均气温与降雨')

#降水分析 ####
rainf <- wth[,c("Date", "Year", "Month","Day","PRE_C")]
str(rainf)
rainf$Rainf <- round(rainf$PRE_C/10, 1)
#Rainfall monthly
wth.monthly <- rainf %>%
  group_by(Year,Month) %>%
  summarise(Rainfall = sum(Rainf))
#2016年各月降水
YearRF <- filter(wth.monthly, Year==2013)
echartr(YearRF, Month, Rainfall, type='vbar') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='月份', axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='降雨量', axisLabel=list(
    formatter='%.0f mm'))%>% 
  setTitle(paste(adrnm,'地块全年各月降水分布',sep=''))
#每日降水
DailyRF <- filter(rainf, Year==2013)

echartr(DailyRF, Date, Rainf, type='vbar') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='月份', axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='降雨量', axisLabel=list(
    formatter='%.0f mm'))%>% 
  setTitle(paste(adrnm,'地块全年降水分布',sep=''))

#温度分析 ####
tp <- wth[,c("Date", "Year", "Month","Day","TEM_M","TEM_H","TEM_L")]
tp$TEM_Mean <- tp$TEM_M/10
tp$TEM_Max <- tp$TEM_H/10
tp$TEM_Min <- tp$TEM_L/10
tp[,c(5:7)] <- NULL
# Year of 2016 with different temp type ####
tp2 <- filter(tp,Year %in% c("2013"))
library(reshape)
tp2l <- melt(tp2, id.var = c("Date","Year","Month","Day"))
names(tp2l)[6] <- "Tempreture"

tp2l$variable <- factor(tp2l$variable, labels=c("平均气温", "最高温", "最低温"))

echartr(tp2l, Date, Tempreture, variable, type='line') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='温度', axisLabel=list(
    formatter='%.0f ℃')) %>% 
  setLegend(pos=12) %>% 
  setTitle(paste(adrnm,'地块温度变化',sep=''))

