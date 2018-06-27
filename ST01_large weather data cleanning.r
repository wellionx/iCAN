setwd("C:/Users/my/Desktop")
library(data.table)
WeatherDT <- fread("T_Main_Weather_Station_Data_OVERALL.txt", sep = "\t",header = FALSE, na.strings = "NA") 

# 查看文件前几行
head(WeatherDT)
dim(WeatherDT)
length(unique(WeatherDT$V2)) # numbers of stationID == 839
length(unique(WeatherDT$V3)) # numbers of Years ==43
#### 去除无关数据 ####
# V31-V37都是无效数据，可以移除

WeatherDT[, c("V31","V32", "V33", "V34", "V35", "V36", "V37") := NULL]

head(WeatherDT)

#### data clean 处理特殊数值####
# 国家级地面观测站历史日值数据，在历史气象日值数据处理过程中，针对降水量特征值处理方式如下：

# 1. 32700一般作为0mm进行处理；
#
# 2. 32XXX（除32744、32766外）的处理结果为（32XXX-32000）*0.1mm
#
# 3. 31XXX的处理结果为（31XXX-31000）*0.1mm
#
# 4. 30XXX的处理结果为（30XXX-30000）*0.1mm
#
# 5. 32766为数据缺测、32744为无数据，如出现，在统计前筛除。
#### clean all the data ####
summary(WeatherDT)
WeatherDT[WeatherDT=="32700"] <- 0
WeatherDT[WeatherDT=="32766"] <- NA
WeatherDT[WeatherDT=="32744"] <- NA

#test with V17
DT = WeatherDT$V17
# 用下标
DT[which(DT > 32000)] <- DT[which(DT > 32000)]-32000
summary(DT)
# 对原数据进行操作,提取相应数值的下标，减掉32000或31000或31000
WeatherDT$V17[which(WeatherDT$V17 > 32000)] <- WeatherDT$V17[which(WeatherDT$V17 > 32000)] - 32000
WeatherDT$V17[which(WeatherDT$V17 > 31000)] <- WeatherDT$V17[which(WeatherDT$V17 > 31000)] - 31000
WeatherDT$V17[which(WeatherDT$V17 > 30000)] <- WeatherDT$V17[which(WeatherDT$V17 > 30000)] - 30000

WeatherDT$V18[which(WeatherDT$V18 > 32000)] <- WeatherDT$V18[which(WeatherDT$V18 > 32000)] - 32000
WeatherDT$V18[which(WeatherDT$V18 > 31000)] <- WeatherDT$V18[which(WeatherDT$V18 > 31000)] - 31000
WeatherDT$V18[which(WeatherDT$V18 > 30000)] <- WeatherDT$V18[which(WeatherDT$V18 > 30000)] - 30000

WeatherDT$V19[which(WeatherDT$V19 > 32000)] <- WeatherDT$V19[which(WeatherDT$V19 > 32000)] - 32000
WeatherDT$V19[which(WeatherDT$V19 > 31000)] <- WeatherDT$V19[which(WeatherDT$V19 > 31000)] - 31000
WeatherDT$V19[which(WeatherDT$V19 > 30000)] <- WeatherDT$V19[which(WeatherDT$V19 > 30000)] - 30000

summary(WeatherDT)

WeatherDT$V17[which(WeatherDT$V17 == 30000)] <- 0
#### 数据瘦身，V6，V7，V8经纬度和海拔可以去掉（站点说明中有相关）####
WeatherDT[, c("V6","V7", "V8") := NULL]
save(WeatherDT, file = "Weather of all clean.rda")

##### pick up year of 2014 data ####
DT <- WeatherDT[V3 == "2014"]
head(DT)

# data type
DT[DT=="32700"]<-0
DT[DT==32766]<-NA
DT[which(DT==32744)]<-NA
# PRE of three type
DT$V17 <- as.integer(DT$V17)
summary(DT$V17)
summary(DT$V18)
summary(DT$V19)
# 没有32XXX，31XXX以及30XXX的数据
# 对个站点的气象数据进行年度平均
library(plyr)
head(DT)
names(DT)
# calculate mean weather of year-monthly
ymdt <- ddply(DT, .(V2,V3,V4), summarise,
            mmPRS_M = round(mean(na.omit(V9)),0),
            mmPRS_H = round(mean(na.omit(V10)),0),
            mmPRS_L = round(mean(na.omit(V11)),0),
            mmTEM_M = round(mean(na.omit(V12)),0),
            mmTEM_H = round(mean(na.omit(V13)),0),
            mmTEM_L = round(mean(na.omit(V14)),0),
            mmRHU_M = round(mean(na.omit(V15)),0),
            mmRHU_L = round(mean(na.omit(V16)),0),
            mmPRE_A = round(mean(na.omit(V17)),0),
            mmPRE_B = round(mean(na.omit(V18)),0),
            mmPRE_C = round(mean(na.omit(V19)),0),
            mmEVP_A = round(mean(na.omit(V20)),0),
            mmEVP_B = round(mean(na.omit(V21)),0),
            mmWIN_A = round(mean(na.omit(V22)),0),
            mmWIN_B = round(mean(na.omit(V23)),0),
            mmWIN_C = round(mean(na.omit(V24)),0),
            mmWIN_D = round(mean(na.omit(V25)),0),
            mmWIN_E = round(mean(na.omit(V26)),0),
            mmSSD = round(mean(na.omit(V27)),0),
            mmGST_A = round(mean(na.omit(V28)),0),
            mmGST_B = round(mean(na.omit(V29)),0),
            mmGST_C = round(mean(na.omit(V30)),0))
# yearly mean weather 
mmdt <- ddply(DT, .(V2,V3), summarise,
              mmPRS_M = round(mean(na.omit(V9)),0),
              mmPRS_H = round(mean(na.omit(V10)),0),
              mmPRS_L = round(mean(na.omit(V11)),0),
              mmTEM_M = round(mean(na.omit(V12)),0),
              mmTEM_H = round(mean(na.omit(V13)),0),
              mmTEM_L = round(mean(na.omit(V14)),0),
              mmRHU_M = round(mean(na.omit(V15)),0),
              mmRHU_L = round(mean(na.omit(V16)),0),
              mmPRE_A = round(mean(na.omit(V17)),0),
              mmPRE_B = round(mean(na.omit(V18)),0),
              mmPRE_C = round(mean(na.omit(V19)),0),
              mmEVP_A = round(mean(na.omit(V20)),0),
              mmEVP_B = round(mean(na.omit(V21)),0),
              mmWIN_A = round(mean(na.omit(V22)),0),
              mmWIN_B = round(mean(na.omit(V23)),0),
              mmWIN_C = round(mean(na.omit(V24)),0),
              mmWIN_D = round(mean(na.omit(V25)),0),
              mmWIN_E = round(mean(na.omit(V26)),0),
              mmSSD = round(mean(na.omit(V27)),0),
              mmGST_A = round(mean(na.omit(V28)),0),
              mmGST_B = round(mean(na.omit(V29)),0),
              mmGST_C = round(mean(na.omit(V30)),0))

# 选取10年的数据进行清洗和预处理
factor(WeatherDT$V3)
DT <- WeatherDT[V3 %in% c("2010","2011")]

head(DT)
factor(DT$V3)
# data type
DT[DT=="32700"]<-0
DT[DT==32766]<-NA
DT[which(DT==32744)]<-NA
# PRE of three type
DT$V17 <- as.integer(DT$V17)
summary(DT$V17)
summary(DT$V18)
summary(DT$V19)

# V2为站点编号，V12平均气温，V13最高气温，V14 最低气温
Weatemp <- WeatherDT[,.(V2,V3,V4,V5,V12,V13,V14)]

head(Weatemp)

write.csv(Weatemp, "weather temperature.csv")

# 选择stationID这列中包含值A或C的所有行

DT1 <- WeatherDT[stationID %in% c("51334","51238")]

dim(DT1)

# 移除"operater"和"description"列

DT1[, c("operater","description") := NULL]

dim(DT1)

DT1[,c("R1","R2","R3","R4","R5"):= NULL]
head(DT1)

write.csv(DT1,"weather station 51334&51238.csv") # 伊犁察布查尔

# 移除"operater"和"description"列
WeatherDT[, c("operater","description") := NULL]
WeatherDT[,c("R1","R2","R3","R4","R5"):= NULL]

dim(WeatherDT)

.libPaths()

DT2 <- WeatherDT[stationID == "54171"]

head(DT2)

write.csv(DT2, "weather station 54171")

DT3 <- WeatherDT[stationID == "54249"]

summary(WeatherDT$V3)
class(WeatherDT$V21)
