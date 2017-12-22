# Connect to MySQL
require(RMySQL)
con<-dbConnect(MySQL(),host='101.200.190.18',port=3306,
               dbname="db_weather",user="root",password="admin") 
summary(con)  
dbGetInfo(con) 
dbListTables(con)

#用SQL语句查询dbGetQuery()和dbSendQuery()两种方法  
wth17 = numeric(0)
res <- dbSendQuery(con, "SELECT * FROM `T_Main_Weather_Station_Data_2017_04` WHERE Station_Id_C=54171;")  
res <- dbSendQuery(con, "SELECT * FROM `T_Main_Weather_Station_Data_2017_05` WHERE Station_Id_C=54171;")
res <- dbSendQuery(con, "SELECT * FROM `T_Main_Weather_Station_Data_2017_06` WHERE Station_Id_C=54171;")
res <- dbSendQuery(con, "SELECT * FROM `T_Main_Weather_Station_Data_2017_07` WHERE Station_Id_C=54171;")
res8 <- dbSendQuery(con, "SELECT * FROM `T_Main_Weather_Station_Data_2017_08` WHERE Station_Id_C=54171;")
res9 <- dbSendQuery(con, "SELECT * FROM `T_Main_Weather_Station_Data_2017_09` WHERE Station_Id_C=54171;")
res10 <- dbSendQuery(con, "SELECT * FROM `T_Main_Weather_Station_Data_2017_10` WHERE Station_Id_C=54171;")
res11 <- dbSendQuery(con, "SELECT * FROM `T_Main_Weather_Station_Data_2017_11` WHERE Station_Id_C=54171;")
res12 <- dbSendQuery(con, "SELECT * FROM `T_Main_Weather_Station_Data_2017_12` WHERE Station_Id_C=54171;")
# data <- dbFetch(res, n=2) #取前2条数据，n=-1时是获取所有数据  
data <- dbFetch(res12, n=-1) #取余下所有数据 
head(data) 
dim(data)

wth17 <- rbind(wth17,data)
unique(wth17$Mon)
 
dbClearResult(res)  
dbDisconnect(con) #断开连接 

save(wth17, file = "weather of JL2017.rda")

#batch selection ####
# Connect to MySQL
require(RMySQL)
con<-dbConnect(MySQL(),host='101.200.190.18',port=3306,
               dbname="db_weather",user="root",password="admin") 
summary(con)  
dbGetInfo(con) 
dbListTables(con)

#iteration with datafile names
nms <- dbListTables(con)
wth017 <- numeric(0)
for(i in 1:13){
  con<-dbConnect(MySQL(),host='101.200.190.18',port=3306,
                 dbname="db_weather",user="root",password="admin") 
  
  res <- dbSendQuery(con, paste("SELECT * FROM", nms[i], "WHERE Station_Id_C=51238;")) 
  data <- dbFetch(res, n=-1) #取余下所有数据 
  head(data) 
  dim(data)
  wth017 <- rbind(wth017,data)
  unique(wth017$Mon)
  dbClearResult(res)  
  dbDisconnect(con) #断开连接 
}

#数据保存与重读取 ####
#数据库读取的均为字符变量，保存table，再读取
write.table(wth017,file = "weather of ChX2017.txt",row.names = F)
wth <- read.table("weather of ChX2017.txt",header = T)

wth[,24:30] <- NULL

#detect outliers
plot(wth$PRE_1h,type="h",ylab="rainfall / mm",xlab = "time by hourly")

#deal with some missing data 999999 ####
summary(wth)

wth[which(wth=="999999",arr.ind = T)] <- NA
wth[which(wth=="999017",arr.ind = T)] <- NA
wth[which(wth=="999990",arr.ind = T)] <- NA

summary(wth)

wth$Date <- as.Date(paste(wth$YEAR, wth$Mon, wth$Day, sep='-'))

### use plyr to summarize the hourly data to daily ####
library(plyr)
dt <- ddply(wth, .(Date), summarise,
            TEM_M = round(mean(na.omit(TEM)),1),
            Tmax = round(max(na.omit(TEM_Max)),1),
            Tmin = round(min(na.omit(TEM_Min)),1),
            humidity = round(mean(na.omit(RHU)),0),
            rainfall = round(sum(na.omit(PRE_1h)),1))
plot(dt$Date,dt$TEM_M,type = 'h', col = "blue")
plot(dt$TEM_M~dt$Date,type = 'o') # outliers found
summary(dt)

# daily rainfall of April and May ####
library(recharts)
require(dplyr)
#add DOY to the data
library(weather)
dt$DOY <- doyFromDate(dt$Date)

#mix plot of tempreture and rainfall
plot(dt$Date, dt$TEM_M,type = 'l')
dt$Month <- monthFromDate(dt$Date)
dt$Day <- dayFromDate(dt$Date)
dt$Year <- yearFromDate(dt$Date)

rf <-filter(dt,Month %in% c("4","5")) 
rf$Month <- factor(rf$Month, labels=c("Apr","May"))
rf$Day <- as.factor(rf$Day)
rf$Date <- as.factor(rf$Date)
echartr(rf, Day, rainfall, t=Month,type='line') %>%
  setTitle('Daily Rainfall of April and May') %>% 
  setSymbols('emptycircle')

echartr(rf, Date, rainfall,type='column') %>%
  setTitle('Daily Rainfall of April and May') %>% 
  setSymbols('emptycircle')
write.csv(dt,file = "daily weather of ChX2017.csv",row.names = F)

#查询历史气息数据####
# 连接数据库服务器查找速度巨慢，还是通过本地查找吧