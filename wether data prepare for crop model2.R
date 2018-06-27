# prepare data for weather computation
library(data.table)
library(plyr)
setwd("C:/Users/my/Desktop/积温计算")

#引入平均温度空间插值后的数据
load( file = "TEM_MHL interpolation.rda")
head(TEM_MHL)
load(file="PRE interpolation.rda")
head(PRE)
load(file="SSD interpolation.rda")
head(SSD)
#EVP <- wth2[,c("stationID","Date","EVP")]
#save(EVP,file="EVP without missing.rda")
load(file="EVP without missing.rda")
head(EVP)

load(file = "RHU interpolation.rda")
head(RHU)
####挑选出太阳辐射(日照时长），最高温，最低温，相对湿度，降水和蒸散量####
wth_model <- merge(SSD, TEM_MHL, by= c("stationID", "Date"))
wth_model <- merge(wth_model,RHU, by= c("stationID", "Date"))
wth_model <- merge(wth_model,PRE, by= c("stationID", "Date"))
wth_model <- merge(wth_model,EVP, by= c("stationID", "Date"))

#weather data prepare for crop model
setwd("E:/Projects in archive/weather visualization")
load(file = "Weather of all clean.rda")
DT <- WeatherDT[V2 %in% "50468"]  # 爱辉
DT <- WeatherDT[V2 %in% c("50557")]  # 嫩江
DT <- WeatherDT[V2 %in% c("50656")]  # 北安
DT <- WeatherDT[V2 %in% c("50756")]  # 海伦
DT <- WeatherDT[V2 %in% c("50658")]  # 克山
DT <- WeatherDT[V2 %in% c("50963")]  # tonghe
DT <- WeatherDT[V2 %in% c("54094")]  # mudanjiang
DT <- WeatherDT[V2 %in% c("54363")]  # 通化
DT <- WeatherDT[V2 %in% c("50745")]  # 通化
dt <- read.csv("同一适宜生态区站点选择.csv",skip = 1)
DT <- WeatherDT[V2 %in% dt[9,"stationID"]]  # dt[9,"stationID"]
DT <- WeatherDT[V2 %in% dt[10,"stationID"]] 
DT <- WeatherDT[V2 %in% dt[11,"stationID"]] 


ii <- 17
getwth <- function{}
DT <- WeatherDT[V2 %in% dt[ii,"stationID"]] 

wth <- DT
colnames(wth) <- c("UID","stationID","Year","Month","Day",
                   "PRS_M","PRS_H","PRS_L","TEM_M","TEM_H","TEM_L","RHU_M","RHU_L",
                   "PRE_A","PRE_B","PRE_C","EVP_A","EVP_B","WIN_A","WIN_B","WIN_C","WIN_D","WIN_E",
                   "SSD","GST_A","GST_B","GST_C")

wth$Date <- as.Date(paste(wth$Year, wth$Month, wth$Day, sep='-'))
# EVP_A plus EVP_B 
wth$EVP_A[is.na(wth$EVP_A)] <- 0
wth$EVP_B[is.na(wth$EVP_B)] <- 0
wth$EVP <- wth$EVP_A + wth$EVP_B

head(wth)
####挑选出太阳辐射(日照时长），最高温，最低温，相对湿度，降水和蒸散量####

# wth2 <- ddply(wth, .(Date, stationID), summarise,
#               
#               Year = Year,
#               solarhr = round(SSD/10,1),
#               Tmax = round(TEM_H/10,1),
#               Tmin = round(TEM_L/10,1),
#               humidity = RHU_M,
#               rainfall = round(PRE_C/10,1),
#               ET = round(EVP/10,1))
wth2 <- wth[,list(Year = Year,
                  solarhr = round(SSD/10,1),
                  Tmax = round(TEM_H/10,1),
                  Tmin = round(TEM_L/10,1),
                  humidity = RHU_M,
                  rainfall = round(PRE_C/10,1),
                  ET = round(EVP/10,1)),
            by=.(stationID,Date)]
# get the Day of Year
library(weather)
wth2$DOY <- doyFromDate(wth2$Date)
head(wth2)
# drop the stationID
# wth2$stationID <- NULL
#check missing data ####
library(mice)
im <- wth2
anyNA(im)
md.pattern(im) 
 
str(im)
im$humidity <- as.numeric(im$humidity)

#KNN插值
library(DMwR)
im <- wth2[,-c(1,2,3)]
knnOutput <- knnImputation(im)  # 使用KNN插值.
anyNA(knnOutput)
wth2_im <- cbind(wth2[,c(1:3)],knnOutput)

wth_final <- wth2_im[,c("Year", "DOY", "solarhr", "Tmax", "Tmin",
                        "humidity" , "rainfall" , "ET")]

# wth_final <- wth2[,c("Year", "DOY", "solarhr", "Tmax", "Tmin",
#                         "humidity" , "rainfall" , "ET")]

# setwd("C:/Users/my/Desktop/get weather data")
wth2_im[,1:6]
tp <- dt[ii,"type"]
stn <- dt[ii,"stationID"]
fn <- paste(tp,"_",stn, " wth file for crop model.csv",sep = "")
fn
write.csv(wth_final,file = fn,row.names = FALSE)

