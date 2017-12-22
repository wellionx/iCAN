# weather station nearby ####
library(leafletCN)
library(leaflet)
# load the station related data
stnearby <- function(longitude,latitude){
  library(leafletCN)
  library(leaflet)
  load(file = "D:/Data/datav/weather station related.rda")
  pop <- paste("气象站名称：",stc$mc,"<br/>",
               
               "ID：",stc$stationID,"<br/>",
               
               "省份：",stc$province,"<br/>",
               
               "经度：",stc$longitude,"<br/>",
               
               "纬度：",stc$latitude,"<br/>")
  # 项目地块展示 #
  leaflet(stc)%>%amap()%>%
    addMarkers(popup=pop) %>%
    addCircleMarkers(lng = longitude,lat = latitude,
                     color="red") %>%
    setView(lng = longitude,lat = latitude,zoom = 8) 
}
# function for look up nearby weather station ####

stnearby(83.13, 46.65) #tacheng

stnearby(81.15, 43.7) #chaxian

stnearby(80.93, 44.02) #huocheng

load(file = "D:/Data/datav/weather station related.rda")
load(file = "station cluster.rda")
stc <- merge(st,dd,by="stationID")
st <- stc

#### 新疆项目地块xj1 ####
leaflet(stc)%>%amap()%>%
  addMarkers(popup=pop) %>%
  addCircleMarkers(lng = 83.13,lat = 46.65,
                   color="red") %>%
  setView(83.13,46.65,zoom = 8) 
# xj2
leaflet(stc)%>%amap()%>%
  addMarkers(popup=pop) %>%
  addCircleMarkers(lng = 83.39,lat = 46.47,
                   color="red") %>%
  setView(lng = 83.39,lat = 46.47,zoom = 8)

# xj3
leaflet(stc)%>%amap()%>%
  addMarkers(popup=pop) %>%
  addCircleMarkers(lng = 82.95,lat = 46.35,
                   color="red") %>%
  setView(lng = 82.95,lat = 46.35, zoom = 8)

xj3bk <-read.delim("clipboard",header = F)
names(xj3bk) <- c("site","longitude","latitude")
pop2 <- paste("地块名称：",xj3bk$site,"<br/>",
             
             "经度：",xj3bk$longitude,"<br/>",
             
             "纬度：",xj3bk$latitude,"<br/>")
# 
leaflet(stc)%>%amap()%>%
  addMarkers(popup=pop) %>%
  addCircleMarkers(lng = xj3bk$longitude,
                   lat = xj3bk$latitude,
                   color="red") %>%
  setView(lng = 82.95,lat = 46.35, zoom = 8)

#location of 察县 and 霍城 ####
leaflet(stc)%>%amap()%>%
  addMarkers(popup=pop) %>%
  addCircleMarkers(lng = c(81.15,80.93),lat = c(43.7,44.02),
                   color="red") %>%
  setView(lng = 81.15,lat = 43.7, zoom = 8)
#loacation of Jilin ####
leaflet(stc)%>%amap()%>%
  addMarkers(popup=pop) %>%
  addCircleMarkers(lng = c(126.35,126.36,126.30,126.29,126.26,126.12),
                   lat = c(43.58, 43.58, 44.04, 44.01, 44.00, 43.56),
                   color="red") %>%
  setView(lng = 126.35,lat = 43.58, zoom = 8)
