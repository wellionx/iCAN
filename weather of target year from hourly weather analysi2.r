#target year weather select day by day
#get hourly weather of specific station from the server

# Connect to MySQL
require(RMySQL)
con<-dbConnect(MySQL(),host='101.200.190.18',port=3306,
               dbname="db_weather",user="root",password="admin") 
summary(con)  
dbGetInfo(con) 
dbListTables(con)[14:19]

#用SQL语句查询dbGetQuery()和dbSendQuery()两种方法  

#Input the stationID
#气象要素温度，降水，湿度，地温等
#最新月份数据
res <- dbSendQuery(con, "SELECT Station_Id_C, YEAR, Mon, Day, Hour, WIN_S_Avg_10mi, TEM, TEM_Max, TEM_Min, RHU, PRE_1h
                   FROM `T_Main_Weather_Station_Data_2018_06` 
                   WHERE Station_Id_C in (51133,51076,50778,50658,50742,54254,54260,54134,53336,53446,54063,51238);")
weanew <- dbFetch(res, n=-1) #取余下所有数据 
#全月数据已更新的
res <- dbSendQuery(con, "SELECT Station_Id_C, YEAR, Mon, Day, Hour, WIN_S_Avg_10mi,TEM, TEM_Max, TEM_Min, RHU, PRE_1h
                   FROM `T_Main_Weather_Station_Data_2018_05` 
                   WHERE Station_Id_C in (51133,51076,50778,50658,50742,54254,54260,54134,53336,53446,54063,51238);")
wea05 <- dbFetch(res, n=-1) #取余下所有数据 
res <- dbSendQuery(con, "SELECT Station_Id_C, YEAR, Mon, Day, Hour, WIN_S_Avg_10mi,TEM, TEM_Max, TEM_Min, RHU, PRE_1h
                   FROM `T_Main_Weather_Station_Data_2018_01`
                         WHERE Station_Id_C in (51133,51076,50778,50658,50742,54254,54260,54134,53336,53446,54063,51238);")
wea01 <- dbFetch(res, n=-1) #取余下所有数据 

res <- dbSendQuery(con, "SELECT Station_Id_C, YEAR, Mon, Day, Hour, WIN_S_Avg_10mi,TEM, TEM_Max, TEM_Min, RHU, PRE_1h
                   FROM `T_Main_Weather_Station_Data_2018_02` 
                   WHERE Station_Id_C in (51133,51076,50778,50658,50742,54254,54260,54134,53336,53446,54063,51238);")
wea02 <- dbFetch(res, n=-1) #取余下所有数据 
res <- dbSendQuery(con, "SELECT Station_Id_C, YEAR, Mon, Day, Hour, WIN_S_Avg_10mi,TEM, TEM_Max, TEM_Min, RHU, PRE_1h
                   FROM `T_Main_Weather_Station_Data_2018_03` 
                    WHERE Station_Id_C in (51133,51076,50778,50658,50742,54254,54260,54134,53336,53446,54063,51238);")
wea03 <- dbFetch(res, n=-1) #取余下所有数据 

res <- dbSendQuery(con, "SELECT Station_Id_C, YEAR, Mon, Day, Hour, WIN_S_Avg_10mi,TEM, TEM_Max, TEM_Min, RHU, PRE_1h
                   FROM `T_Main_Weather_Station_Data_2018_04` 
                    WHERE Station_Id_C in (51133,51076,50778,50658,50742,54254,54260,54134,53336,53446,54063,51238);")
wea04 <- dbFetch(res, n=-1) #取余下所有数据 

dbClearResult(res)  
dbDisconnect(con) #断开连接 
# data <- dbFetch(res, n=2) #取前2条数据，n=-1时是获取所有数据  

wth18 = numeric(0)
wth18 <- rbind(wea01,wea02)
wth18 <- rbind(wth18, wea03)
wth18 <- rbind(wth18, wea04)
wth18 <- rbind(wth18, wea05)
wth18 <- rbind(wth18, weanew)

stid <- "sites12"
fn <- paste("weather in 2018@station ", stid,".txt", sep = '')
write.table(wth18, file=fn, row.names = FALSE,quote = FALSE)

library(data.table)
wth18 <- fread(file=fn,header=T)
unique(wth18$Mon)
str(wth18)
summary(wth18)

##分组统计观测值
wth18[,.N, by = .(Station_Id_C, Mon)]

#查看某些站点缺失数据有多少天，只取某一个小时的数据
wth18[Hour=="8"][,.N, by=.(Station_Id_C, Mon)]

table(wth18[Station_Id_C=="54134"&Mon=="1"&Hour=="8"]$Day)

#check missing data ####
#999999	缺测，指应当观测而实际未观测的数据
wth18[which(wth18=="999999",arr.ind = T)] <- NA
#999990	微量
wth18[which(wth18=="999990",arr.ind = T)] <- 0
library(mice)
anyNA(wth18)
md.pattern(wth18) 

#select 4 hour(2,8,14,20)to calc mean daily hour
#在气象学上日平均气温是在一天24小时当中取4个时间段的气温来平均，这4个时间段分别为2时、8时、14时、20时，把这4个时间段的气温相加后再除以4就能得出该日的日平均气温。
#平均相对湿度计算=四次定时的平均值
library(dplyr)
library(data.table)

F4pts <- wth18[Hour %in% c("2","8","14","20")]

library(plyr)
TEM_RH <- F4pts[, list(TEM_M = round(mean(TEM),1),
                       RHU_M=round(mean(RHU),1)),
                by=.(Station_Id_C,YEAR,Mon,Day)]

TEMX_PRE <- wth18[,list(TEM_Max=max(TEM_Max),
                        TEM_Min=min(TEM_Min),
                        WIN = round(mean(WIN_S_Avg_10mi),1),
                        PRE = sum(na.omit(PRE_1h))),
                  by=.(Station_Id_C,YEAR,Mon,Day)]

wth.dly <- merge(TEM_RH, TEMX_PRE, by = c("Station_Id_C","YEAR","Mon","Day"))
str(wth.dly)

wth.dly$Date <- as.Date(paste(wth.dly$YEAR,wth.dly$Mon, wth.dly$Day, sep = '-'))
setDT(wth.dly)
setkey(wth.dly,Date)
colnames(wth.dly)[2:3] <- c("Year", "Month")

save(wth.dly, file="daily weather of year 2018.rda")
load(file="daily weather of year 2018.rda")

#single station
# Connect to MySQL
require(RMySQL)
con<-dbConnect(MySQL(),host='101.200.190.18',port=3306,
               dbname="db_weather",user="root",password="admin") 
summary(con)  
dbGetInfo(con) 
dbListTables(con)[14:19]
res <- dbSendQuery(con, "SELECT Station_Id_C, YEAR, Mon, Day, Hour, WIN_S_Avg_10mi, TEM, TEM_Max, TEM_Min, RHU, PRE_1h
                   FROM `T_Main_Weather_Station_Data_2018_06` 
                   WHERE Station_Id_C=54135;")
weanew <- dbFetch(res, n=-1) #取余下所有数据 
#全月数据已更新的
res <- dbSendQuery(con, "SELECT Station_Id_C, YEAR, Mon, Day, Hour, WIN_S_Avg_10mi,TEM, TEM_Max, TEM_Min, RHU, PRE_1h
                   FROM `T_Main_Weather_Station_Data_2018_05` 
                   WHERE Station_Id_C=54135;")
wea05 <- dbFetch(res, n=-1) #取余下所有数据 
res <- dbSendQuery(con, "SELECT Station_Id_C, YEAR, Mon, Day, Hour, WIN_S_Avg_10mi,TEM, TEM_Max, TEM_Min, RHU, PRE_1h
                   FROM `T_Main_Weather_Station_Data_2018_01`
                   WHERE Station_Id_C=54135;")
wea01 <- dbFetch(res, n=-1) #取余下所有数据 

res <- dbSendQuery(con, "SELECT Station_Id_C, YEAR, Mon, Day, Hour, WIN_S_Avg_10mi,TEM, TEM_Max, TEM_Min, RHU, PRE_1h
                   FROM `T_Main_Weather_Station_Data_2018_02` 
                   WHERE Station_Id_C=54135;")
wea02 <- dbFetch(res, n=-1) #取余下所有数据 
res <- dbSendQuery(con, "SELECT Station_Id_C, YEAR, Mon, Day, Hour, WIN_S_Avg_10mi,TEM, TEM_Max, TEM_Min, RHU, PRE_1h
                   FROM `T_Main_Weather_Station_Data_2018_03` 
                   WHERE Station_Id_C=54135;")
wea03 <- dbFetch(res, n=-1) #取余下所有数据 

res <- dbSendQuery(con, "SELECT Station_Id_C, YEAR, Mon, Day, Hour, WIN_S_Avg_10mi,TEM, TEM_Max, TEM_Min, RHU, PRE_1h
                   FROM `T_Main_Weather_Station_Data_2018_04` 
                   WHERE Station_Id_C=54135;")
wea04 <- dbFetch(res, n=-1) #取余下所有数据 

dbClearResult(res)  
dbDisconnect(con) #断开连接 
# data <- dbFetch(res, n=2) #取前2条数据，n=-1时是获取所有数据  

wth18 = numeric(0)
wth18 <- rbind(wea01,wea02)
wth18 <- rbind(wth18, wea03)
wth18 <- rbind(wth18, wea04)
wth18 <- rbind(wth18, wea05)
wth18 <- rbind(wth18, weanew)

stid <- "sites12"
fn <- paste("weather in 2018@station ", stid,".txt", sep = '')
write.table(wth18, file=fn, row.names = FALSE,quote = FALSE)

library(data.table)
wth18 <- fread(file=fn,header=T)
unique(wth18$Mon)
str(wth18)
summary(wth18)

##分组统计观测值
wth18[,.N, by = .(Station_Id_C, Mon)]

#查看某些站点缺失数据有多少天，只取某一个小时的数据
wth18[Hour=="8"][,.N, by=.(Station_Id_C, Mon)]

table(wth18[Station_Id_C=="54134"&Mon=="1"&Hour=="8"]$Day)

#check missing data ####
#999999	缺测，指应当观测而实际未观测的数据
wth18[which(wth18=="999999",arr.ind = T)] <- NA
#999990	微量
wth18[which(wth18=="999990",arr.ind = T)] <- 0
library(mice)
anyNA(wth18)
md.pattern(wth18) 

#select 4 hour(2,8,14,20)to calc mean daily hour
#在气象学上日平均气温是在一天24小时当中取4个时间段的气温来平均，这4个时间段分别为2时、8时、14时、20时，把这4个时间段的气温相加后再除以4就能得出该日的日平均气温。
#平均相对湿度计算=四次定时的平均值
library(dplyr)
library(data.table)

F4pts <- wth18[Hour %in% c("2","8","14","20")]

library(plyr)
TEM_RH <- F4pts[, list(TEM_M = round(mean(TEM),1),
                       RHU_M=round(mean(RHU),1)),
                by=.(Station_Id_C,YEAR,Mon,Day)]

TEMX_PRE <- wth18[,list(TEM_Max=max(TEM_Max),
                        TEM_Min=min(TEM_Min),
                        WIN = round(mean(WIN_S_Avg_10mi),1),
                        PRE = sum(na.omit(PRE_1h))),
                  by=.(Station_Id_C,YEAR,Mon,Day)]

wth.dly <- merge(TEM_RH, TEMX_PRE, by = c("Station_Id_C","YEAR","Mon","Day"))
str(wth.dly)

wth.dly$Date <- as.Date(paste(wth.dly$YEAR,wth.dly$Mon, wth.dly$Day, sep = '-'))
setDT(wth.dly)
setkey(wth.dly,Date)
colnames(wth.dly)[2:3] <- c("Year", "Month")

# get the Day of Year
doyFromDate <- function(date) {
  date <- as.character(date)
  as.numeric(format(as.Date(date), "%j"))
}
wth.dly$DOY <- doyFromDate(wth.dly$Date)
write.csv(wth.dly,file="daily weather of year2018 @54135.csv")


