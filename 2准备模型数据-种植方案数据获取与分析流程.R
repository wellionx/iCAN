load(file = "D:\\Data\\datav\\Weather of 30 years clean data to2017.rda")
#pick up the station we need 
# two stations in Xinjiang
library(data.table)
DT <- DT30ys[stationID %in% c("51133")]  # tacheng

DT <- DT30ys[stationID %in% c("51076")]  # alatai
DT <- DT30ys[stationID %in% c("54254")]  # kaiyuan

DT <- DT30ys[stationID %in% c("54260")]  # xifeng

DT <- DT30ys[stationID %in% c("53446")]  # Neimeng

DT <- DT30ys[stationID %in% c("53336")]  # Neimeng 巴彦淖尔乌兰特旗

DT <- DT30ys[stationID %in% c("54134")]  # Neimeng 集丰粮贸-巴彦淖尔乌兰特旗

#中种国际黑龙江四地市
DT <- DT30ys[stationID %in% c("50658")]  # 拜泉县
DT <- DT30ys[stationID %in% c("50742")]  # 富裕县
DT <- DT30ys[stationID %in% c("50778")]  # 同江市
DT <- DT30ys[stationID %in% c("50788")]  # 富锦市

#察县
DT <- DT30ys[stationID %in% c("51238")]  # 察县
DT <- DT30ys[stationID %in% c("54416")]  # 密云

#通辽通辽花吐古拉镇
DT <- DT30ys[stationID %in% c("54135")]

wth <- DT

wth$Date <- as.Date(paste(wth$Year, wth$Month, wth$Day, sep='-'))
# EVP_A plus EVP_B 
wth$EVP_A[is.na(wth$EVP_A)] <- 0
wth$EVP_B[is.na(wth$EVP_B)] <- 0
wth$EVP <- wth$EVP_A + wth$EVP_B

head(wth)
####挑选出太阳辐射(日照时长），最高温，最低温，相对湿度，降水和蒸散量####
library(plyr)
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
                  ET =  round(EVP/10,1)),
            by=.(stationID,Date)]
setkey(wth2,Date) #order the Date
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
anyNA(wth2)
md.pattern(wth2) 
#if there is missing value, use KNN to impute the missing
#if not missing value ,skip this process
#KNN插值
library(DMwR)
im <- wth2[,-1]
knnOutput <- knnImputation(im)  # 使用KNN插值.
anyNA(knnOutput)
wth2_im <- cbind(wth2[,1],knnOutput)
#data with missing values
wth_final <- wth2_im[,c("Year", "DOY", "solarhr", "Tmax", "Tmin",
                        "humidity" , "rainfall" , "ET")]

#data output without missing values
wth_final <- wth2[,c("Year", "DOY", "solarhr", "Tmax", "Tmin",
                        "humidity" , "rainfall" , "ET")]

#rename the data, and save to txt files for wth data
#save files to the desktop
write.table(wth_final,file = "C:/Users/Wei Li/Desktop/wth file TLhuatugula for crop model20180625.txt",sep = "\t",row.names = FALSE,quote = FALSE)
