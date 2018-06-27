load(file = "D:\\Data\\datav\\Weather of 30 years clean data to2017.rda")
#pick up the station we need - history weather
library(data.table)
DT <- DT30ys[stationID %in% c("51133")]  # tacheng
#DT <- DT30ys[stationID %in% c("50742")]  # 富裕县
#four parameters for distance calculate
DT$Date <- as.Date(paste(DT$Year, DT$Month, DT$Day, sep='-'))
setkey(DT, Date )
head(DT)
#method 1 - calculate all the day parameter in April
wth <- DT[,c("Year","Month","Day","TEM_M","WIN_A","TEM_H","TEM_L", "PRE_C","RHU_M","Date")]
#check NAs
anyNA(wth)
summary(wth)
colnames(wth)[5:8]<- c("WIN","TEM_Max","TEM_Min","PRE")
wth2 <- cbind(wth[,c(1:3,10,9)],wth[,4:8]/10)

#load target year to current weather
load(file="daily weather of year 2018.rda")
#target month
tar <- wth.dly
summary(tar)
tar <- tar[Station_Id_C %in% c("51133")]
tar[,"Station_Id_C"] <- NULL

wthall <- rbind(wth2, tar)

# similar history Year day by day ####
k=10
kn.ix <- NULL # create a variable to track row index the Year

for(m in unique(tar$Month)){
  
  tarmon <- tar[Month %in% m]
  
  for (d in unique(tarmon$Day)){
    dtmon <- wthall[Month %in% m]
    dtday <- dtmon[Day %in% d]
    setDF(dtday)
    rownames(dtday) <- paste("Year",seq(1988,2018,1),sep = '')
    df.scaled <- scale (dtday[,5:10]) # Data standardization
    dist.eucl <- dist(df.scaled, method = "euclidean" )
    distance <- as.matrix(dist.eucl)
    distSort <- sort(distance[,"Year2018"], ind=TRUE) 
    tmp.ix <- distSort$ix[1:k] # temporary index variable for the loop
    kn.ix <- c(kn.ix, tmp.ix) # append row index of stations for each location
    
  }
}

Year = 1988 + kn.ix -1
sort(table(Year),decreasing = T)

#KNN 算法改进，采用7日滑动均值作为气象数据
#SMA顾名思义就是简单移动平均，其主要是用来进行简单的求平均值，移动平均方法是收集一组观察值，计算这组观察值的均值，利用这一均值作为下一期的预测值
library(TTR)
wthall$TEM_Mave7 <- SMA(wthall$TEM_M,n=7)
wthall$RHU_Mave7 <- SMA(wthall$RHU_M,n=7)
wthall$TEM_Maxave7 <- SMA(wthall$TEM_Max,n=7)
wthall$TEM_Minave7 <- SMA(wthall$TEM_Min,n=7)
wthall$PREave7 <- SMA(wthall$PRE,n=7)
# similar history Year day by day ####
k=10
kn.ix <- NULL # create a variable to track row index the Year

for(m in unique(tar$Month)){
  
  tarmon <- tar[Month %in% m]
  
  for (d in unique(tarmon$Day)){
    dtmon <- wthall[Month %in% m]
    dtday <- dtmon[Day %in% d]
    setDF(dtday)
    rownames(dtday) <- paste("Year",seq(1988,2018,1),sep = '')
    df.scaled <- scale (dtday[,10:14]) # Data standardization
    dist.eucl <- dist(df.scaled, method = "euclidean" )
    distance <- as.matrix(dist.eucl)
    distSort <- sort(distance[,"Year2018"], ind=TRUE) 
    tmp.ix <- distSort$ix[1:k] # temporary index variable for the loop
    kn.ix <- c(kn.ix, tmp.ix) # append row index of stations for each location
    
  }
}

Year = 1988 + kn.ix -1
sort(table(Year), decreasing = T)

#历史相似年份为2002年，单年气象数据可视化 ####
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

DailyWD <- filter(windy, Year==2002)
#全年风速，分析“秋风大” ####
echartr(DailyWD, monthday, WIN, type="curve")%>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', splitNumber=7, axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='风速', splitNumber=5, axisLabel=list(
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
gdrn <- filter(dt,Year %in% "2002")

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
detach("package:plyr") #卸除，否则与dplyr冲突
wth.monthly <- rainf %>%
  group_by(Year,Month) %>%
  summarise(Rainfall = sum(Rainf))
#2016年各月降水
YearRF <- filter(wth.monthly, Year==2002)
echartr(YearRF, Month, Rainfall, type='vbar') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='月份', axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='降雨量', axisLabel=list(
    formatter='%.0f mm'))%>% 
  setTitle(paste(adrnm,'地块全年各月降水分布',sep=''))
#每日降水
DailyRF <- filter(rainf, Year==2002)
DailyRF$monthday <- paste(DailyRF$Month,DailyRF$Day,sep="-" )
echartr(DailyRF, monthday, Rainf, type='vbar') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', axisLabel=list(
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
tp2 <- filter(tp,Year %in% c("2002"))
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


#今年 + 历史相似年
head(wthall)

wth <- wthall
adrnm <- "塔城"

wth$Date <- as.Date(paste(wth$Year, wth$Month, wth$Day, sep='-'))
wth$Date <- factor(wth$Date)
wth$Year <- factor(wth$Year)
wth$Month <- factor(wth$Month)
wth$Day <- factor(wth$Day)
wth$monthday <- paste(wth$Month, wth$Day, sep = "-")
#秋风分析 ####
windy <- wth[,c("Date", "Year", "Month","Day","monthday","WIN")]
windy

library(recharts)
setkey(windy, Date)
setDF(windy)
#数据按照日期进行排序
DailyWD <- filter(windy, Year == 2013 | Year==2018)
str(DailyWD)
DailyWD$Year <- factor(DailyWD$Year)
DailyWD$variable <- factor(DailyWD$Year, labels=c("历史相似年", "今年"))
#全年风速，分析“秋风大” ####
echartr(DailyWD, monthday, WIN,variable, type="curve")%>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', splitNumber=7, axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='风速', splitNumber=5, axisLabel=list(
    formatter='%.0f m/s'))%>% 
  setLegend(pos=12) %>% 
  setTitle(paste(adrnm,'地块风速变化',sep=''))

#播种决策，地温和降水 ####
#look at the two month of April and May
grdtp_rf <- wth[,c("Date", "Year", "Month","Day","TEM_M","PRE")]

library(dplyr)
dt <- filter(grdtp_rf,Month %in% c("4","5"))
dt$TEM <- dt$TEM_M
dt$rainfall <- dt$PRE
# Combine ground temp with rainfall
gdrn <- filter(dt,Year %in% "2018")

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
rainf <- wth[,c("Date", "Year", "Month","Day","monthday","PRE")]
str(rainf)

#Rainfall monthly
require(dplyr)
wth.monthly <- rainf %>%
  group_by(Year, Month) %>%
  summarise(Rainfall = sum(PRE))

#2016年各月降水
YearRF <- filter(wth.monthly, Year==2002 |Year==2018 )
YearRF$variable <- factor(YearRF$Year, labels=c("历史相似年", "今年"))
echartr(YearRF, Month, Rainfall, variable,  type='vbar') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='月份', axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='降雨量', axisLabel=list(
    formatter='%.0f mm'))%>% 
  setLegend(pos=12) %>% 
  setTitle(paste(adrnm,'地块全年各月降水分布',sep=''))
#每日降水
DailyRF <- filter(rainf, Year==2002 |Year==2018)
DailyRF$variable <- factor(DailyRF$Year, labels=c("历史相似年", "今年"))

echartr(DailyRF, monthday, PRE,variable, type='vbar') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='降雨量', axisLabel=list(
    formatter='%.0f mm'))%>% 
  setLegend(pos=12) %>% 
  setTitle(paste(adrnm,'地块全年降水分布',sep=''))


#温度分析 ####
tp <- wth[,c("Date", "Year", "Month","Day","TEM_M","TEM_Max","TEM_Min")]
tp$TEM_Mean <- tp$TEM_M

tp[,c(5)] <- NULL

tp22 <- filter(tp,Year %in% c("2002","2018"))
tp22$variable <- factor(tp22$Year, labels=c("历史相似年", "今年"))
tp22$Year <- factor(tp22$Year)
tp22$monthday <- paste(tp22$Month, tp22$Day, sep = "-")
echartr(tp22, monthday, TEM_Mean, variable, type='line') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='温度', axisLabel=list(
    formatter='%.0f ℃')) %>% 
  setLegend(pos=12) %>% 
  setTitle(paste(adrnm,'地块平均气温变化',sep=''))

echartr(tp22, monthday, TEM_Min, variable, type='line') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='温度', axisLabel=list(
    formatter='%.0f ℃')) %>% 
  setLegend(pos=12) %>% 
  setTitle(paste(adrnm,'地块最低气温变化',sep=''))
