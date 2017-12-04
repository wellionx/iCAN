library(recharts)
# save(ne3rf1,ne3rf2,ne3rf678, file = "rainfall of ne3 stations.rda")
load(file = "Weather of all clean.rda")

# choose one station in Jilin #54161,changchun
chch <- WeatherDT[V2=="54161"]
temp <- chch[,.(V3, V4, V5,V12, V13, V14)]
head(temp)
names(temp) <- c("Year","Month","Day","TEM_M","TEM_H","TEM_L")
# months of 56789
library(dplyr)
tp <-filter(temp,Month %in% c("5","6","7","8","9")) 
tp$Year <- factor(tp$Year)
tp$Month <- factor(tp$Month, labels=c("May", "Jun", "Jul", "Aug", "Sep"))
tp$Day <- as.character(tp$Day)
tp$TEM_M <- 0.1*(tp$TEM_M)
tp$TEM_H <- 0.1*(tp$TEM_H)
tp$TEM_L <- 0.1*(tp$TEM_L)
head(tp)

# set Year in order ####
tp <-tp[with(tp, order(Year)), ]
echartr(tp, Day, TEM_M, Month, t= Year,type='line') %>%
  setTitle('Changchun Temperature May - Sep, by Month') %>% 
  setSymbols('emptycircle')

# Year of 2016 with different temp type ####
tp2 <- filter(tp,Year %in% c("2016"))
library(reshape)
tp2l <- melt(tp2, id.var = c("Year","Month","Day"))
names(tp2l)[5] <- "Tempreture"

tp2l$variable <- factor(tp2l$variable, labels=c("Media", "Highest", "Lowest"))
echartr(tp2l, Day, Tempreture, variable,t=Month, type='line') %>%
  setTitle('Changchun Temperature May - Sep 2016, by Month') %>% 
  setSymbols('emptycircle')
