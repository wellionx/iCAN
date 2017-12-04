load(file = "rainfall of ne3 stations.rda")
library(ggplot2)
p <- ggplot(ne3rf678, aes(Year, Rainfall, fill = Month)) + 
  geom_bar(stat = "identity") + 
  xlab("Year") + 
  ylab("Rainfall / mm") +
  theme_light()
####修改 X 轴刻度文字的大小、字体、颜色、加粗、位置、角度：
p <- p + theme(axis.text.x = element_text(size = 10,vjust = 1, hjust = 1, angle = 45))
# 修改坐标轴的刻度间隔
p + scale_y_continuous(breaks=seq(0, 600, 100)) 

