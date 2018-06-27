load("D:\\Data\\datav\\Weather of 30 years clean data to2017.rda")
#pick up the station we need 
library(data.table)
library(dplyr)
DT <- DT30ys[stationID %in% c("51133")]  # tacheng

DT <- DT30ys[stationID %in% c("51076")]  # alatai

DT <- DT30ys[stationID %in% c("54260")]  # xifeng

DT <- DT30ys[stationID %in% c("53446")]  # Neimeng 巴彦淖尔

DT <- DT30ys[stationID %in% c("54226")]  # 通辽奈曼旗集丰粮贸

DT <- DT30ys[stationID %in% c("53336")]  # Neimeng 巴彦淖尔乌兰特旗

DT <- DT30ys[stationID %in% c("54416")]  # miyun

#中种国际黑龙江四地市
DT <- DT30ys[stationID %in% c("50658")]  # 拜泉县
DT <- DT30ys[stationID %in% c("50742")]  # 富裕县
DT <- DT30ys[stationID %in% c("50778")]  # 同江市向阳乡
DT <- DT30ys[stationID %in% c("50788")]  # 富锦市农资大市场旁

#吉林地块
DT <- DT30ys[stationID %in% c("54063")]  # 吉林金珠乡
head(DT)

wth <- DT
adrnm <- "吉林金珠乡"

wth$Date <- as.Date(paste(wth$Year, wth$Month, wth$Day, sep='-'))
wth$Date <- factor(wth$Date)
wth$Year <- factor(wth$Year)
wth$Month <- factor(wth$Month)
wth$Day <- factor(wth$Day)


#秋风分析 ####
windy <- wth[,c("Date", "Year", "Month","Day","WIN_A")]
windy

library(recharts)

#第一次无法画出图，查看有无确实数据

#填补缺失值后仍然无法画图
#数据类型不对，转换成dataframe
setkey(windy, Date)
setDF(windy)
#数据按照日期进行排序
windy$monthday <- paste(windy$Month, windy$Day, sep='-')
windy$WIN <- round(windy$WIN_A/10,1)
echartr(windy, monthday, WIN, t=Year, type="curve")
#全年风速
echartr(windy, monthday, WIN, Year, type="curve")%>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', splitNumber=7, axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='风速', splitNumber=8, axisLabel=list(
    formatter='%.0f m/s'))%>% 
  #setTitle('塔城地区近30年6-7月份风速变化')
  setTitle(paste(adrnm,'地块近30年全年风速变化',sep=''))

library(dplyr)
win <-filter(windy, Month %in% c("4","5","6","7","8","9")) 
win$Month <- factor(win$Month, labels=c("Apr","May", "Jun", "Jul", "Aug", "Sep"))

echartr(win, Day, WIN, Year, t=Month, type='curve') %>% setToolbox(pos=3)
#单独看6-7月份的风速
win67 <- filter(windy, Month %in% c("6","7")) 

echartr(win67, monthday, WIN, Year, type='curve') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', splitNumber=7, axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='风速', splitNumber=8, axisLabel=list(
    formatter='%.0f m/s'))%>% 
  #setTitle('塔城地区近30年6-7月份风速变化')
  setTitle('集丰粮贸地块近30年6-7月份风速变化')

#单独看8-9月份的风速
win89 <- filter(windy, Month %in% c("8","9")) 

echartr(win89, monthday, WIN, Year, type='curve') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', splitNumber=7, axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='风速', axisLabel=list(
    formatter='%.0f m/s'))%>% 
  #setTitle('塔城地区近30年8-9月份风速变化')
  setTitle('集丰粮贸地块近30年8-9月份风速变化')

#早霜分析（秋天地表温度低于0℃） ####
grdst<- wth[,c("Date", "Year", "Month","Day","GST_C")]
str(grdst)

grdst$monthday <- paste(grdst$Month, grdst$Day, sep='-')

gst890 <- filter(grdst, Month %in% c("8","9","10")) 
gst890$GST <- gst890$GST_C/10
echartr(gst890, monthday, GST, Year, type='curve') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', splitNumber=11, axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='地表温度', splitNumber=5, axisLabel=list(
    formatter='%.0f ℃'))%>% 
  setTitle(paste(adrnm,'地块近30年8-10月份地表温度',sep=''))



#降水分析 ####
rainf <- wth[,c("Date", "Year", "Month","Day","PRE_C")]
str(rainf)

rainf$Rainf <- round(rainf$PRE_C/10, 1)
# Rainfall yearly
require(dplyr)
library(recharts)
wth.yearly <- rainf %>%
  group_by(Year) %>%
  summarise(Rainfs = sum(Rainf))

echartr(wth.yearly, Year, Rainfs,type='vbar') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='年份', axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='降雨量', axisLabel=list(
    formatter='%.0f mm'))%>% 
  setTitle(paste(adrnm,'地块30年全年降水分析',sep=''))

#Rainfall monthly
wth.monthly <- rainf %>%
  group_by(Year,Month) %>%
  summarise(Rainfall = sum(Rainf))
# 带时间轴的条图Bar Chart with Timeline
# options(repr.plot.width=4, repr.plot.height=3)
echartr(wth.monthly, Month, Rainfall, t=Year, type='vbar') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='月份', axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='降雨量', axisLabel=list(
    formatter='%.0f mm'))%>% 
  setTitle(paste(adrnm,'地块全年各月降水分布',sep=''))


#30年平均个月降水
RF.meamonth <-  wth.monthly %>%
  group_by(Month) %>%
  summarise(mRainfall = mean(Rainfall))
echartr(RF.meamonth, Month, mRainfall, type='vbar') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='月份', axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='降雨量', axisLabel=list(
    formatter='%.0f mm'))%>% 
  setTitle(paste(adrnm,'地块全年各月降水分布',sep=''))
#May-Aug rainfall
crtRain <- filter(wth.monthly,Month %in% c("5","6","7","8","9"))
#取子集后factor数量没有变化，需要对取子集的变量重新factor
crtRain$Month <- factor(crtRain$Month)
echartr(crtRain, Year, Rainfall, Month,type='column', subtype='stack') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='年份')%>% 
  setYAxis(name='降雨量', splitNumber=5, axisLabel=list(
    formatter='%.0f mm'))%>% 
  setTitle(paste(adrnm,'地块近30年5-8月份累积降雨量',sep=''))

#全年地表温度
grdst$GST <- grdst$GST_C/10
setDF(grdst)
echartr(grdst, monthday, GST, Year, type='curve') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', splitNumber=7, axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='地表温度', splitNumber=5, axisLabel=list(
    formatter='%.0f ℃'))%>% 
  #setTitle('塔城地区近30年8-10月份地表温度')
  setTitle(paste(adrnm,'近30年全年地表温度',sep=''))

#播种决策，地温和降水 ####
#look at the two month of April and May
grdtp_rf <- wth[,c("Date", "Year", "Month","Day","TEM_M","GST_C","PRE_C")]
dt <- filter(grdtp_rf,Month %in% c("4","5"))
dt$GST <- dt$GST_C/10
dt$rainfall <- dt$PRE_C/10
dt$TEM <- dt$TEM_M/10
# Combine ground temp with rainfall
gdrn <- filter(dt,Year %in% "2016")

# library(reshape)
# gdrn1 <- melt(gdrn, id=c('Month', 'Day'), measure=c('GST', 'rainfall'), 
#               variable='Param')
# gdrn1$monthday <- paste(gdrn1$Month, gdrn1$Day, sep = "-")

#用中文变量名
gdrn$地表温度 <- gdrn$GST
gdrn$降水量 <- gdrn$rainfall

library(reshape)
gdrn2 <- melt(gdrn, id=c('Month', 'Day'), measure=c('地表温度', '降水量'), 
              variable='Param')

gdrn2$monthday <- paste(gdrn2$Month, gdrn2$Day, sep = "-")

g2 <- echartr(gdrn2, monthday, value, Param, type='curve')

g2 %>% setY1Axis(series='降水量', axisLine=list(lineStyle=lineStyle(width=0)), 
                 name='降水量', axisLabel=list(formatter="%.1f mm"), min=0,max=30) %>%
  setXAxis(name="日期",splitLine=list(show=FALSE), axisLine=list(
    lineStyle=lineStyle(color='darkgray'))) %>% 
  setYAxis(axisLine=list(lineStyle=lineStyle(width=0)), 
           axisLabel=list(formatter="%.1f °C"), name='地表温度') %>% 
  setGrid(borderWidth=0) %>% 
  setToolbox(pos=3) %>%
  setLegend(pos = 12) %>%
  setTitle('2016年玉米播种期前后地表温度与降雨')


#地表温度变化不稳的时，查看近5年平均地温和气温
grdtp_rf <- wth[,c("Date", "Year", "Month","Day","TEM_M","GST_C","PRE_C")]
dt <- filter(grdtp_rf,Month %in% c("4","5"))
dt$GST <- dt$GST_C/10
dt$rainfall <- dt$PRE_C/10
dt$TEM <- dt$TEM_M/10
grdn5 <- filter(dt,Year %in% c("2017","2016","2015","2014","2013"),
                Month %in% c("4","5"))

library(plyr)
grdn5ave <- ddply(grdn5, .(Month,Day), summarise,
              mGST = round(mean(GST),1),
              mTEM = round(mean(na.omit(TEM)),1))

grdn5ave$monthday <- paste(grdn5ave$Month, grdn5ave$Day, sep = "-")
echartr(grdn5ave, monthday, mTEM, type='curve') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', splitNumber=7, axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='温度', splitNumber=5, axisLabel=list(
    formatter='%.0f ℃'))%>% 
  #setTitle(paste(adrnm,'地块近5年4-5月地表温度变化',sep=''))
  setTitle(paste(adrnm,'地块近5年4-5月平均气温变化',sep=''))

#look at the two month of April and May in each year
library(reshape)
gdrn <- melt(dt, id=c('Year','Month', 'Day'), measure=c('GST', 'rainfall'), 
             variable='Param')
gdrn$monthday <- paste(gdrn$Month, gdrn$Day, sep = "-")
#order the variable
gdrn <- gdrn[with(gdrn, order(Year,Month,Day)), ]

g4 <- echartr(gdrn, monthday, value, Param,t=Year, type='curve')

g4 %>% setY1Axis(series='rainfall', axisLine=list(lineStyle=lineStyle(width=0)), 
                 name='降水量', axisLabel=list(formatter="%.1f mm"), max=30) %>%
  setXAxis(name="日期",splitLine=list(show=FALSE), axisLine=list(
    lineStyle=lineStyle(color='darkgray'))) %>% 
  setYAxis(axisLine=list(lineStyle=lineStyle(width=0)), 
           axisLabel=list(formatter="%.1f °C"), max=30, name='地表温度') %>% 
  setGrid(borderWidth=0) %>% 
  setToolbox(pos=3) %>%
  setTitle('Ground Temperature and Rainfall in history') 
#Apr-May rainfall prob analysis 
#以降水概率高的日期为播种日期
rf45 <- dt[,c("Year","Month","Day","rainfall")]
rf45$monthday <- paste(rf45$Month, rf45$Day, sep = "-")
rf45$RfPrb <- ifelse(rf45$rainfall==0,0,1)
echartr(rf45, monthday, RfPrb, Year, type='area', subtype='stack') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', splitNumber=7, axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name="Count", axisLabel=list(
    formatter='%.0f'))%>% 
  setTitle(paste(adrnm,'地块近30年4-5月份降水概率',sep=''))


#温度分析
temp <- wth[,c("Date", "Year", "Month","Day","TEM_M")]
str(temp)

temp$TEM <- round(temp$TEM_M/10, 1)
setDF(temp)
temp$monthday <- paste(temp$Month, temp$Day, sep='-')
#全年平均气温变化
echartr(temp, monthday, TEM, Year,  type='curve') %>% 
  setToolbox(pos=3) %>% 
  setXAxis(name='日期', splitNumber=7, axisLabel=list(
    formatter='%m/%d'))%>% 
  setYAxis(name='气温', axisLabel=list(
    formatter='%.0f ℃'))%>% 
  setTitle(paste(adrnm,'地块30年全年平均气温变化',sep=''))


tp <-filter(temp, Month %in% c("4","5","6","7","8","9")) 


setDF(tp)
tp$Month <- factor(tp$Month)
echartr(tp, Day, TEM, Year, t=Month, type='curve') %>% 
  setToolbox(pos=3) %>% 
  setTitle('不同年份气温变化') 

tp$monthday <- paste(tp$Month, tp$Day, sep='-')
echartr(tp, monthday, TEM, Year,  type='curve') %>% 
  setToolbox(pos=3) %>% 
  setTitle('不同年份气温变化') 

