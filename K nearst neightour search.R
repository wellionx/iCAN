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

#Geocode by Baidu Map API

BaiduGeocode <- function(address) {
#建立备用向量，包括空向量及百度地图api秘钥
baidu_lng <- c()
baidu_lat <- c()
ak <- 'hRMTft6Y932u2N5UX2cokQ8GvOE86UQl'    #百度地图api的秘钥，需自己申请
#加载包
library(rjson)
library(RCurl)
#循环解析过程
for (location in address) {
  #生成规则的url地址(具体参数可参考Geocoding API文档)
  url <- paste('http://api.map.baidu.com/geocoder/v2/?ak=',ak,'&callback=renderOption&output=json&address=',location,sep='')
  #利用URLencode()转换为可解析的URL地址
  url_string <- URLencode(url)
  #通过readLines读取URL地址，并解析JSON格式的结果
  json<- readLines(url_string, warn=F)
  geo <- fromJSON(substr(json,regexpr('\\(',json)+1,nchar(json)-1))
  #在解析结果中提取经纬度
  lng<-geo$result$location$lng
  lat<-geo$result$location$lat
  #存储到已经建好的字段中
  baidu_lng <- c(baidu_lng,lng)
  baidu_lat <- c(baidu_lat,lat)
}
#整理结果
result <- data.frame(address=address,longitude=baidu_lng,latitude=baidu_lat)
return(result)
}

#地理位置→经纬度####
address <- read.table('address_list.txt',header = F, col.names=c("address"),as.is = c(1))
address <- address$address   #转化为向量格式，备for循环使用
head(address)

# Find geocodes of each city
kNStations <- function(address.list, station.list, k = 5)
{
  # Find geocodes of each city
  coords <- BaiduGeocode(address)
  
  # Loop through each location, finding the k-nearest stations from the main list:
  kns.ix <- NULL # create a variable to track row index the stations
  kns.dist <- NULL # create a distance variable
  for(i in 1:length(city.list))
  {
    # Great cirle distance calculator:
    dist <- gcd.slc(coords$longitude[i], coords$latitude[i], station.list$longitude, station.list$latitude)
    distSort <- sort(dist, ind=TRUE) 
    tmp.ix <- distSort$ix[1:k] # temporary index variable for the loop
    tmp.dist <- distSort$x[1:k] # temporary distance variable for the loop
    kns.ix <- c(kns.ix, tmp.ix) # append row index of stations for each location
    kns.dist <- c(kns.dist, tmp.dist) # append distances of stations for each location
  }
  st <- station.list[kns.ix,] # Subset the full list with k-nearest stations
  st$city <- rep(city.list, each=k) # Insert reference City
  st$Ref_Lat <- rep(coords$latitude,each=k) # Insert reference Latitude
  st$Ref_Lon <- rep(coords$longitude, each=k) # Insert reference Longitude
  
  st$kilo_distance <- kns.dist # Insert distance into result
  st <- st[with(st,order(city, kilo_distance)),]
  st$rank <- rep(1:k,length(city.list)) # Rank closest to farthest (1 to k)
  
  # Queries are made to NOAA database by year:
  st$BEGIN_Year <- as.numeric(substr(st$BEGIN,1,4)) # Start Year
  st$END_Year <- as.numeric(substr(st$END, 1, 4)) # End Year
  return(st)
}


kNStations(address, weast, k=5)
