# prepare data for weather computation
library(data.table)
library(plyr)
load(file = "Weather of all clean.rda")

#pick up the station we need 
# two stations in Xinjiang
DT <- WeatherDT[V2 %in% c("51133")]  # tacheng
DT2 <- WeatherDT[V2 %in% c("51238")] # chaxian
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

wth2 <- ddply(wth, .(Date, stationID), summarise,
              
              Year = Year,
              solarhr = round(SSD/10,1),
              Tmax = round(TEM_H/10,1),
              Tmin = round(TEM_L/10,1),
              humidity = RHU_M,
              rainfall = round(PRE_C/10,1),
              ET = round(EVP/10,1))
# get the Day of Year
library(weather)
wth2$DOY <- doyFromDate(wth2$Date)
head(wth2)
# drop the stationID
wth2$stationID <- NULL
#check missing data ####
library(mice)
md.pattern(wth2) 


write.csv(wth2,file = "wth file for crop model.csv",row.names = FALSE)

write.csv(wth_chx,file = "wth file Chx2016s for crop model.csv",row.names = FALSE)

write.csv(wth_tch,file = "wth file tch2016s for crop model.csv",row.names = FALSE)
