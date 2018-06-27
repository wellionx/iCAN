load("D:\\Data\\datav\\Weather of 30 years clean data to2017.rda")

#pick up the station we need 
library(data.table)
library(dplyr)

#mean temp of year 2017
DT <- DT30ys[stationID %in% c("51133")]  # tacheng
DT <- DT30ys[stationID %in% c("50658")]  # 拜泉县
DT <- DT30ys[stationID %in% c("53446")]  # Neimengbayannaoer


wth <- DT[Year %in% "2016"]

wth$Date <- as.Date(paste(wth$Year, wth$Month, wth$Day, sep='-'))
wth$Date <- factor(wth$Date)
tem <- wth[,c("Date", "Year", "Month","Day","TEM_H","TEM_L")]

tem$TEM_Max <- tem$TEM_H/10
tem$TEM_Min <- tem$TEM_L/10

write.csv(tem, file = "Max and Min tempreture in 2016@50658.csv")


#calc GDD of each station 
tem <- wth.dly[,c("Station_Id_C", "Year", "Month","Day","Date","TEM_Max","TEM_Min")]
tem$Tmean <- (tem$TEM_Max + tem$TEM_Min)/2
tem$Alpha <- (tem$TEM_Max - tem$TEM_Min)/2

TU=34
TL=10
#Case 1
subdata <- subset(tem, TEM_Max > TU)
subdata <- subset(subdata, TEM_Min < TL)

subdata$Thta1 <- asin((TL-subdata$Tmean)/subdata$Alpha)
subdata$Thta2 <- asin((TU-subdata$Tmean)/subdata$Alpha)

subdata$GDD <- ((subdata$Tmean-TL)*(subdata$Thta2-subdata$Thta1) + subdata$Alpha*(cos(subdata$Thta1)-cos(subdata$Thta2)) + 
               (TU-TL)*(pi/2-subdata$Thta2))/pi
subdata1 <- subdata

#Case2
subdata <- subset(tem, TEM_Max > TU)
subdata <- subset(subdata, TEM_Min > TL)

subdata$Thta2 <- asin((TU-subdata$Tmean)/subdata$Alpha)

subdata$GDD <- ((subdata$Tmean-TL)*(subdata$Thta2+pi/2) + 
                  (TU-TL)*(pi/2-subdata$Thta2)-cos(subdata$Thta2))/pi

subdata2 <- subdata

#Case3
subdata <- subset(tem, TEM_Max < TU)
subdata <- subset(subdata, TEM_Min > TL)

subdata$GDD <- subdata$Tmean- TL

subdata3 <- subdata

#Case4
subdata <- subset(tem, TEM_Max < TU & TEM_Max > TL)
subdata <- subset(subdata, TEM_Min < TL)

subdata$Thta1 <- asin((TL-subdata$Tmean)/subdata$Alpha)

subdata$GDD <- ((subdata$Tmean-TL)*(pi/2-subdata$Thta1) + subdata$Alpha*cos(subdata$Thta1))/pi

subdata4 <- subdata
#Case5
subdata <- subset(tem, TEM_Max > TU & TEM_Min > TU)
subdata$GDD <- TU-TL

subdata5 <- subdata
#Case6
subdata <- subset(tem, TEM_Max < TL)
subdata$GDD <- 0

subdata6 <- subdata

gdd <- rbind(subdata1[,c(1:8,13)],subdata2[,c(1:8,12)])
gdd <- rbind(gdd, subdata3[,c(1:8,11)])
gdd <- rbind(gdd, subdata4[,c(1:8,12)])
gdd <- rbind(gdd, subdata5[,c(1:8,11)])
gdd <- rbind(gdd, subdata6[,c(1:8,11)])

setkey(gdd, Date)

sum(gdd$GDD)

#calc gdd from daily weather 2018
gdd <- rbind(subdata1[,c(1:8,12)],subdata2[,c(1:8,11)])
gdd <- rbind(gdd, subdata3[,c(1:8,10)])
gdd <- rbind(gdd, subdata4[,c(1:8,11)])
gdd <- rbind(gdd, subdata5[,c(1:8,10)])
gdd <- rbind(gdd, subdata6[,c(1:8,10)])

write.csv(gdd,"GDD of each station.csv")
