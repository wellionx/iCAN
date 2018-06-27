##种植方案的基本流程
#1.根据地块GPS信息锁定附近气象站点 ####

# Nearest Neighbor Search####
#Before we can find the k-nearest stations we need an accurate metric to measure distance between a location’s reference point and each station.

deg2rad <- function(deg) return(deg*pi/180)

gcd.slc <- function(long1, lat1, long2, lat2) {
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)
  R <- 6371 # Earth mean radius in km
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}
# Great cirle distance calculator:
kNStations <- function(coords, station.list, k = 5){
  kns.ix <- NULL # create a variable to track row index the stations
  kns.dist <- NULL # create a distance variable  
  dist <- gcd.slc(coords$longitude, coords$latitude, station.list$longitude, station.list$latitude)
  distSort <- sort(dist, ind=TRUE) 
  tmp.ix <- distSort$ix[1:k] # temporary index variable for the loop
  tmp.dist <- distSort$x[1:k] # temporary distance variable for the loop
  kns.ix <- c(kns.ix, tmp.ix) # append row index of stations for each location
  kns.dist <- c(kns.dist, tmp.dist) # append distances of stations for each location

  st <- station.list[kns.ix,1:5] # Subset the full list with k-nearest stations
  st$Ref_Lat <- rep(coords$latitude,each=k) # Insert reference Latitude
  st$Ref_Lon <- rep(coords$longitude, each=k) # Insert reference Longitude

  st$kilo_distance <- kns.dist # Insert distance into result
  st <- st[with(st,order(kilo_distance)),]
  st$rank <- rep(1:k) # Rank closest to farthest (1 to k)

  return (st)
}
#气象站数据读入
weast <- read.csv("D:\\Data\\datav\\stations with weather data.csv")

#通辽奈曼旗 (120.61,42.75)
#巴彦淖尔乌兰特前旗（109.27,41.3)
#集丰粮贸(120.90,43.42)
#北彩村(116.73563,40.159169)
#coord <- data.frame(longitude=88.70685, latitude=46.53878) #阿勒泰地区

#经纬度度分秒转换为十进制
degree=81
minute=23
second=42

decimal=degree + minute/60 + second/(60*60)

coord <- data.frame(longitude=116.73563, latitude=40.159169) #内蒙

coord <- data.frame(longitude=81.395, latitude=43.691) #察县

kns <- kNStations(coord, weast,k=5)

kns

#选择气象站阿勒泰作为备选站点，气象站编号为51076
#选择气象站包头市作为内蒙古巴彦淖尔市数据，编号53446
#选择乌兰特中期站点作为乌兰特旗数据，气象站编号53336
#选择气象站开鲁(编号54134)为集丰粮贸站点