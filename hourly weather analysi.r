near7days <- read.table("S201805020900569988030xj.txt",header = T)
head(near7days)
unique(near7days$Day)
#drop the last row
tail(near7days)
near7days <- near7days[-289,]
unique(near7days$Station_Id_C)
#select 4 hour(2,8,14,20)to calc mean daily hour
#在气象学上日平均气温是在一天24小时当中取4个时间段的气温来平均，这4个时间段分别为2时、8时、14时、20时，把这4个时间段的气温相加后再除以4就能得出该日的日平均气温。
library(dplyr)
tem <- filter(near7days, Hour %in% c("2","8","14","20"))
tem$Date <- as.Date(paste(tem$Year,tem$Mon,tem$Day,sep = "-"))

library(plyr)
tem_m <- ddply(tem, .(Station_Id_C, Date), summarise,
                     TEM_M = round(mean(TEM),1))
library(data.table)
Apr4 <- fread("T_Main_Weather_Station_Data_2018_04.txt",header = T)
Apr4

Apr4s <- Apr4[Station_Id_C %in% c("51133","51076")]
unique(Apr4$Day)
tem <- filter(Apr4s, Hour %in% c("2","8","14","20"))

tem_m <- ddply(tem, .(Station_Id_C, YEAR,Mon,Day), summarise,
               TEM_M = round(mean(TEM),1))
tem_m$Date <- as.Date(paste(tem_m$YEAR,tem_m$Mon,tem_m$Day,sep = "-"))

