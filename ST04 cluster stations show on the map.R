#weather station data
load(file= "weather station.rda") # stmini
load(file= "station cluster.rda") # dd
head(stmini)
head(dd)
stmini$stationID <- as.factor(stmini$stationID)
# station data preparing ####
stc <- merge(st,dd,by="stationID")
# map of China ####
library("leafletCN")
library(leaflet)
demomap("china")
# amap 高德地图底图
leaflet(stc)%>%amap()%>%
  addCircles(popup=stmini)
#聚类结果展示 ####
pal <- colorFactor(palette = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
                   domain = c("1","2","3","4","5"))
leaflet(stc)%>%amap()%>%
  addCircles(color = ~pal(cluster))

# with more different clusters ####
load(file = "weather station clustering variation of 10 years.rda")
load(file= "weather station.rda") # stmini

head(stmini)
head(cc10plus)
cc <- cc10plus


names(cc)[1] <- "stationID"
stmini$stationID <- as.factor(stmini$stationID)
# station data preparing ####
stc <- merge(stmini,cc,by="stationID")
# map of China ####
library("leafletCN")
library(leaflet)
demomap("china")
# amap 高德地图底图
leaflet(stc)%>%amap()%>%
  addCircles(popup=stmini)
#### 标签变量赋值 ####
st <- stc
pop <- paste("气象站名称：",st$mc,"<br/>",
             
             "ID：",st$stationID,"<br/>",
             
             "省份：",st$province,"<br/>",
             
             "经度：",st$longitude,"<br/>",
             
             "纬度：",st$latitude,"<br/>")
leaflet(st)%>%amap()%>%
  addCircles(popup=pop)
#聚类结果展示 ####
# color choice ####
library(RColorBrewer)
display.brewer.pal(9,"Set1")
colors <- brewer.pal(9,"Set1")
#map of 4 clusters
pal <- colorFactor(palette = colors[1:4],
                   domain = unique(stc$cluster_4))
leaflet(stc)%>%amap()%>%
  addCircles(color = ~pal(cluster_4),popup=pop)
#map of 5 clusters
pal <- colorFactor(palette = colors[1:5],
                   domain = unique(stc$cluster_5))
leaflet(stc)%>%amap()%>%
  addCircles(color = ~pal(cluster_5))
#map of 6 clusters
pal <- colorFactor(palette = colors[1:6],
                   domain = unique(stc$cluster_6))
leaflet(stc)%>%amap()%>%
  addCircles(color = ~pal(cluster_6))

#map of 7 clusters ####
pal <- colorFactor(palette = colors[1:7],
                   domain = unique(stc$cluster_7))
leaflet(stc)%>%amap()%>%
  addCircles(color = ~pal(cluster_7))
