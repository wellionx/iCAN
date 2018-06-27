setwd("D:/Data/weather/CMA/中国地面累年值日值数据集(1981-2010年）")

#相对路径
setwd("C:/Users/Wei Li/Desktop/中国地面气候资料日值数据集(V3.0)")

files = list.files(pattern="*.txt")
files

library(data.table)
DT = do.call(rbind, lapply(files, fread,  header= T))

summary(DT)

#pretreatment of weather data
dim(DT)
colnames(DT) <- c("stationID","Valid_time_zone","DOY",
                  "TEM_M","TEM_H","TEM_L", "PRS_M",
                   "PRE_2020","PRE_0808","WIN_M")
DT

# 999999表示缺测，999998表示未观测
DT[DT=="999999"] <- NA

#select a special station
cDT <- DT
weaDT <- DT[stationID == "54254"] #辽宁开原
weaDT <- DT[stationID == "54416"] #北京密云
save(cDT, file = "累积逐日气象数据.rda")

#valid time zone of two or more 
#split the stirng ,pick up the last 4 string as 2010
wea2 <- cDT[stationID == "51058"] #辽宁开原
wea2$lastyear <- substr(wea2$Valid_time_zone, 6,9)

#down load files from CMA
dt <- read.table("S201803291108327497700.txt")
head(dt)
urls <- as.character(dt$V1)
dt <- read.table(file = urls[1])

