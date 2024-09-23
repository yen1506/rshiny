library(shinyWidgets)
library(shiny)
library(sf)
library(tmap)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(showtext)
library(cartography)
library(mapview)
library(systemfonts)
library(timeDate)
library(leaflet)
library(leafpop)
library(shiny)
library(rlist)
library(showtext)
library(sysfonts)
library(tidyr)
library(showtextdb)
library(xts)
library(TSstudio)
library(lubridate)
library(plotly)
library(magrittr)
library(directlabels)
library(shinydashboard)
library(shinyalert)
# mapviewOptions(fgb = F)
showtext_auto(enable = TRUE)
font_add("STHeiti Medium","STHeiti Medium.ttc")
options(encoding = "UTF-8")

###################   Data      ##################################
# setwd("~/Desktop/data visual/final")
#2021
i1 = read.csv("202101_i.csv")
i2 = read.csv("202102_i.csv")
i3 = read.csv("202103_i.csv")
i4 = read.csv("202104_i.csv")
i5 = read.csv("202105_i.csv")
i6 = read.csv("202106_i.csv")
i7 = read.csv("202107_i.csv")
i8 = read.csv("202108_i.csv")
i9 = read.csv("202109_i.csv")
i10 = read.csv("202110_i.csv")
i11 = read.csv("202111_i.csv")

o1 = read.csv("202101_o.csv")
o2 = read.csv("202102_o.csv")
o3 = read.csv("202103_o.csv")
o4 = read.csv("202104_o.csv")
o5 = read.csv("202105_o.csv")
o6 = read.csv("202106_o.csv")
o7 = read.csv("202107_o.csv")
o8 = read.csv("202108_o.csv")
o9 = read.csv("202109_o.csv")
o10 = read.csv("202110_o.csv")
o11 = read.csv("202111_o.csv")
#2020
i01 = read.csv("202001_i.csv")
i02 = read.csv("202002_i.csv")
i03 = read.csv("202003_i.csv")
i04 = read.csv("202004_i.csv")
i05 = read.csv("202005_i.csv")
i06 = read.csv("202006_i.csv")
i07 = read.csv("202007_i.csv")
i08 = read.csv("202008_i.csv")
i09 = read.csv("202009_i.csv")
i010 = read.csv("202010_i.csv")
i011 = read.csv("202011_i.csv")
i012 = read.csv("202012_i.csv")

o01 = read.csv("202001_o.csv")
o02 = read.csv("202002_o.csv")
o03 = read.csv("202003_o.csv")
o04 = read.csv("202004_o.csv")
o05 = read.csv("202005_o.csv")
o06 = read.csv("202006_o.csv")
o07 = read.csv("202007_o.csv")
o08 = read.csv("202008_o.csv")
o09 = read.csv("202009_o.csv")
o010 = read.csv("202010_o.csv")
o011 = read.csv("202011_o.csv")
o012 = read.csv("202012_o.csv")

# i7 = i7 %>% subset(select = -c(X))
# i10 = i10 %>% subset(select = -c(X))
i8 = i8 %>% subset(select = -c(X))
i01 = i01 %>% subset(select = -c(X))
i02 = i02 %>% subset(select = -c(X))
i03 = i03 %>% subset(select = -c(X))
i07 = i07 %>% subset(select = -c(X))
i08 = i08 %>% subset(select = -c(X))
i010 = i010 %>% subset(select = -c(X))
i012 = i012 %>% subset(select = -c(X))
o01 = o01 %>% subset(select = -c(X))
o02 = o02 %>% subset(select = -c(X))
o03 = o03 %>% subset(select = -c(X))
o05 = o05 %>% subset(select = -c(X))
o07 = o07 %>% subset(select = -c(X))
o08 = o08 %>% subset(select = -c(X))
o010 = o010 %>% subset(select = -c(X))
o012 = o012 %>% subset(select = -c(X))
o1 = o1 %>% subset(select = -c(X))
o7 = o7 %>% subset(select = -c(X))
o8 = o8 %>% subset(select = -c(X))
o10 = o10 %>% subset(select = -c(X))

i2020 = rbind(i01,i02,i03,i04,i05,i06,i07,i08,i09,i010,i011,i012)
o2020 = rbind(o01,o02,o03,o04,o05,o06,o07,o08,o09,o010,o011,o012)
i2021 = rbind(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
o2021 = rbind(o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11)
i2020 = i2020[-which(i2020$X....車站.日期==""),]
o2020 = o2020[-which(o2020$X....車站.日期==""),]
i2021 = i2021[-which(i2021$X....車站.日期==""),]
o2021 = o2021[-which(o2021$X....車站.日期==""),]
i2020[,1] = as.Date(i2020[,1])
o2020[,1] = as.Date(o2020[,1])
i2021[,1] = as.Date(i2021[,1])
o2021[,1] = as.Date(o2021[,1])
for (j in 2:length(i2020)){
  i2020[,j] = as.numeric(gsub(",","",i2020[,j]))
}
colnames(i2020)[1] = "Date"
for (j in 2:length(o2020)){
  o2020[,j] = as.numeric(gsub(",","",o2020[,j]))
}
for (j in 2:length(i2021)){
  i2021[,j] = as.numeric(gsub(",","",i2021[,j]))
}
colnames(i2021)[1] = "Date"
for (j in 2:length(o2021)){
  o2021[,j] = as.numeric(gsub(",","",o2021[,j]))
}
colnames(o2020)[1] = "Date"
colnames(i2020)[84] = "台北101世貿"
colnames(o2020)[84] = "台北101世貿"
colnames(o2021)[1] = "Date"
colnames(i2021)[84] = "台北101世貿"
colnames(o2021)[84] = "台北101世貿"

cols = colnames(i2020)[2:length(i2020)]
sum2020 = data.frame(
  Date = as.Date(i2020[,1]),
  i2020[,cols] + o2020[,cols])
cols = colnames(i2021)[2:length(i2021)]
sum2021 = data.frame(
  Date = as.Date(i2021[,1]),
  i2021[,cols] + o2021[,cols])

## total
sums = rbind(sum2020,sum2021)
##in
pass_in = rbind(i2020,i2021)
##out
pass_out = rbind(o2020,o2021)

###################   panel1    ###########################
#########  tab 1 ################
spot = read.csv("result.csv")

spot_sf <- st_as_sf(spot, 
                    coords = c(
                      "StationPosition__PositionLon",
                      "StationPosition__PositionLat"), 
                    crs = 4326)

popup_all <- select(spot_sf, c("StationID","StationName__Zh_tw","StationName__En","LocationCity","LocationTown","Line_Type","StationType"))

map_all = mapview(spot_sf, 
                  label = "StationName__Zh_tw",
                  cex = 4,
                  alpha = 0.1,
                  col.regions = "white",
                  color = "white",
                  layer.name = "捷運站",
                  popup = popupTable(popup_all),
                  homebutton = F)

blue = spot[which(spot$MLineType == "板南線"),]
blue_sf <- st_as_sf(blue, 
                    coords = c(
                      "StationPosition__PositionLon",
                      "StationPosition__PositionLat"), 
                    crs = 4326)
popup_blue <- select(blue_sf, c("StationID","StationName__Zh_tw","StationName__En","LocationCity","LocationTown","Line_Type","StationType"))
map_blue = mapview(blue_sf, 
                  label = "StationName__Zh_tw",
                  cex = 4,
                  alpha = 0.1,
                  col.regions = "blue",
                  color = "blue",
                  layer.name = "板南線",
                  popup = popupTable(popup_blue),
                  homebutton = F)

red = spot[which(spot$MLineType == "淡水信義線"),]
red_sf <- st_as_sf(red, 
                   coords = c("StationPosition__PositionLon", "StationPosition__PositionLat"), crs = 4326)
popup_red <- select(red_sf, c("StationID","StationName__Zh_tw","StationName__En","LocationCity","LocationTown","Line_Type","StationType"))
map_red = mapview(red_sf, 
                   label = "StationName__Zh_tw",
                   cex = 4,
                   alpha = 0.1,
                   col.regions = "red",
                   color = "red",
                   layer.name = "淡水信義線",
                   popup = popupTable(popup_red),
                   homebutton = F)
lred = spot[which(spot$LineType == "淡水信義線（新北投線）"),]
lred_sf <- st_as_sf(lred, 
                   coords = c("StationPosition__PositionLon", "StationPosition__PositionLat"), crs = 4326)
popup_lred <- select(lred_sf, c("StationID","StationName__Zh_tw","StationName__En","LocationCity","LocationTown","Line_Type","StationType"))
map_lred = mapview(lred_sf, 
                  label = "StationName__Zh_tw",
                  cex = 4,
                  alpha = 0.1,
                  col.regions = "pink",
                  color = "pink",
                  layer.name = "新北投支線",
                  popup = popupTable(popup_lred),
                  homebutton = F)

yellow = spot[which(spot$MLineType == "環狀線"),]
yellow_sf <- st_as_sf(yellow, coords = c("StationPosition__PositionLon", "StationPosition__PositionLat"), crs = 4326)
popup_yellow <- select(yellow_sf, c("StationID","StationName__Zh_tw","StationName__En","LocationCity","LocationTown","Line_Type","StationType"))
map_yellow = mapview(yellow_sf, 
                   label = "StationName__Zh_tw",
                   cex = 4,
                   alpha = 0.1,
                   col.regions = "yellow",
                   color = "yellow",
                   layer.name = "環狀線",
                   popup = popupTable(popup_yellow),
                   homebutton = F)
orange = spot[which(spot$MLineType == "中和新蘆線"),]
orange_sf <- st_as_sf(orange, coords = c("StationPosition__PositionLon", "StationPosition__PositionLat"), crs = 4326)
popup_orange <- select(orange_sf, c("StationID","StationName__Zh_tw","StationName__En","LocationCity","LocationTown","Line_Type","StationType"))
map_orange = mapview(orange_sf, 
                   label = "StationName__Zh_tw",
                   cex = 4,
                   alpha = 0.1,
                   col.regions = "orange",
                   color = "orange",
                   layer.name = "中和新蘆線",
                   popup = popupTable(popup_orange),
                   homebutton = F)

brown = spot[which(spot$MLineType == "文湖線"),]
brown_sf <- st_as_sf(brown, coords = c("StationPosition__PositionLon", "StationPosition__PositionLat"), crs = 4326)
popup_brown <- select(brown_sf, c("StationID","StationName__Zh_tw","StationName__En","LocationCity","LocationTown","Line_Type","StationType"))
map_brown = mapview(brown_sf, 
                   label = "StationName__Zh_tw",
                   cex = 4,
                   alpha = 0.1,
                   col.regions = "brown",
                   color = "brown",
                   layer.name = "文湖線",
                   popup = popupTable(popup_brown),
                   homebutton = F)

green = spot[which(spot$MLineType == "松山新店線"),]
green_sf <- st_as_sf(green, coords = c("StationPosition__PositionLon", "StationPosition__PositionLat"), crs = 4326)
popup_green <- select(green_sf, c("StationID","StationName__Zh_tw","StationName__En","LocationCity","LocationTown","Line_Type","StationType"))
map_green = mapview(green_sf,
                   label = "StationName__Zh_tw",
                   cex = 4,
                   alpha = 0.1,
                   col.regions = "darkgreen",
                   color = "darkgreen",
                   layer.name = "松山新店線",
                   popup = popupTable(popup_green),
                   homebutton = F)
lgreen = spot[which(spot$LineType == "松山新店線（小碧潭線）"),]
lgreen_sf <- st_as_sf(lgreen, coords = c("StationPosition__PositionLon", "StationPosition__PositionLat"), crs = 4326)
popup_lgreen <- select(lgreen_sf, c("StationID","StationName__Zh_tw","StationName__En","LocationCity","LocationTown","Line_Type","StationType"))
map_lgreen = mapview(lgreen_sf,
                    label = "StationName__Zh_tw",
                    cex = 4,
                    alpha = 0.1,
                    col.regions = "green",
                    color = "green",
                    layer.name = "小碧潭支線",
                    popup = popupTable(popup_lgreen),
                    homebutton = F)
#########  tab 2 ################

basic = read.csv("basic.csv")

chart11 = ggplot(basic, aes(x = 營運車站數, y=營運車站數, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))+geom_path()

chart11 = ggplotly(chart11)
chart12 = ggplot(basic, aes(x = 營運車站數, y=營運路線數, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart12 = ggplotly(chart12)
chart13 = ggplot(basic, aes(x = 營運車站數, y=營運里程, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart13 = ggplotly(chart13)
chart14 = ggplot(basic, aes(x = 營運車站數, y=客運人次, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart14 = ggplotly(chart14)
chart15 = ggplot(basic, aes(x = 營運車站數, y=客運收入千元, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart15 = ggplotly(chart15)
chart21 = ggplot(basic, aes(x = 營運路線數, y=營運車站數, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart21 = ggplotly(chart21)
chart22 = ggplot(basic, aes(x = 營運路線數, y=營運路線數, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart22 = ggplotly(chart22)
chart23 = ggplot(basic, aes(x = 營運路線數, y=營運里程, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart23 = ggplotly(chart23)
chart24 = ggplot(basic, aes(x = 營運路線數, y=客運人次, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart24 = ggplotly(chart24)
chart25 = ggplot(basic, aes(x = 營運路線數, y=客運收入千元, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart25 = ggplotly(chart25)
chart31 = ggplot(basic, aes(x = 營運里程, y=營運車站數, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart31 = ggplotly(chart31)
chart32 = ggplot(basic, aes(x = 營運里程, y=營運路線數, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart32 = ggplotly(chart32)
chart33 = ggplot(basic, aes(x = 營運里程, y=營運里程, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart33 = ggplotly(chart33)
chart34 = ggplot(basic, aes(x = 營運里程, y=客運人次, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart34 = ggplotly(chart34)
chart35 = ggplot(basic, aes(x = 營運里程, y=客運收入千元, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart35 = ggplotly(chart35)
chart41 = ggplot(basic, aes(x = 客運人次, y=營運車站數, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart41 = ggplotly(chart41)
chart42 = ggplot(basic, aes(x = 客運人次, y=營運路線數, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart42 = ggplotly(chart42)
chart43 = ggplot(basic, aes(x = 客運人次, y=營運里程, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart43 = ggplotly(chart43)
chart44 = ggplot(basic, aes(x = 客運人次, y=客運人次, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart44 = ggplotly(chart44)
chart45 = ggplot(basic, aes(x = 客運人次, y=客運收入千元, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart45 = ggplotly(chart45)
chart51 = ggplot(basic, aes(x = 客運收入千元, y=營運車站數, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart51 = ggplotly(chart51)
chart52 = ggplot(basic, aes(x = 客運收入千元, y=營運路線數, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart52 = ggplotly(chart52)
chart53 = ggplot(basic, aes(x = 客運收入千元, y=營運里程, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart53 = ggplotly(chart53)
chart54 = ggplot(basic, aes(x = 客運收入千元, y=客運人次, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart54 = ggplotly(chart54)
chart55 = ggplot(basic, aes(x = 客運收入千元, y=客運收入千元, color=年別))+geom_point()+geom_path()+theme_bw()+scale_colour_gradientn(colours=rainbow(25))
chart55 = ggplotly(chart55)

###################   panel2    ###########################
####### df #####
#df for boxplot
avg20 = data.frame(
  station = colnames(sum2020)[2:length(colnames(sum2020))],
  person = round(colMeans(sum2020[2:length(sum2020)]),0),
  diff = rep(0,119),
  percent = rep(0,119))
spot.s = spot %>% select(StationName__Zh_tw,MLineType,StationType)

avg2020 = left_join(avg20,spot.s,
                    by = c("station" = "StationName__Zh_tw"))
avg2020 = avg2020 %>% mutate(year = rep(2020,135))

avg21 = data.frame(
  station = colnames(sum2021)[2:length(colnames(sum2021))],
  person = round(colMeans(sum2021[2:length(sum2021)]),0),
  diff = round(colMeans(sum2021[2:length(sum2021)]),0) - avg20$person,
  percent = (round(colMeans(sum2021[2:length(sum2021)]),0) - avg20$person)/avg20$person)
spot.s = spot %>% select(StationName__Zh_tw,MLineType,StationType)

avg2021 = left_join(avg21,spot.s,
                    by = c("station" = "StationName__Zh_tw"))
avg2021 = avg2021 %>% mutate(year = rep(2021,135))

avg = rbind(avg2020,avg2021)
avg$year =  as.character(avg$year)

#df for barplot
av = data.frame(
  station = colnames(sum2020)[2:length(colnames(sum2020))],
  number20 = round(colMeans(sum2020[2:length(sum2020)]),0),
  number21 = round(colMeans(sum2021[2:length(sum2021)]),0),
  diff = round(colMeans(sum2021[2:length(sum2021)]),0) -
    round(colMeans(sum2020[2:length(sum2020)]),0),
  percent = round((round(colMeans(sum2021[2:length(sum2021)]),0) -
                     round(colMeans(sum2020[2:length(sum2020)]),0))/round(colMeans(sum2020[2:length(sum2020)]),0),3)
)
av_m = left_join(av,spot.s, by = c("station" = "StationName__Zh_tw"))

####### barplot #############
## count ##
bar_blue <- plot_ly(av_m[which(av_m$MLineType == "板南線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_blue <- bar_blue %>% add_trace(y = ~number20, name = '2020')
bar_blue <- bar_blue %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

bar_red <- plot_ly(av_m[which(av_m$MLineType == "淡水信義線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_red <- bar_red %>% add_trace(y = ~number20, name = '2020')
bar_red <- bar_red %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

bar_yellow <- plot_ly(av_m[which(av_m$MLineType == "環狀線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_yellow <- bar_yellow %>% add_trace(y = ~number20, name = '2020')
bar_yellow <- bar_yellow %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

bar_green <- plot_ly(av_m[which(av_m$MLineType == "松山新店線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_green <- bar_green %>% add_trace(y = ~number20, name = '2020')
bar_green <- bar_green %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

bar_brown <- plot_ly(av_m[which(av_m$MLineType == "文湖線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_brown <- bar_brown %>% add_trace(y = ~number20, name = '2020')
bar_brown <- bar_brown %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

bar_orange <- plot_ly(av_m[which(av_m$MLineType == "中和新蘆線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_orange <- bar_orange %>% add_trace(y = ~number20, name = '2020')
bar_orange <- bar_orange %>% layout(yaxis = list(title = 'Count'), barmode = 'group')


bar_transfer_blue <- plot_ly(av_m[which(av_m$StationType == "轉乘站" &av_m$MLineType == "板南線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_transfer_blue <- bar_transfer_blue %>% add_trace(y = ~number20, name = '2020')
bar_transfer_blue <- bar_transfer_blue %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

bar_transfer_red <- plot_ly(av_m[which(av_m$StationType == "轉乘站" &av_m$MLineType == "淡水信義線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_transfer_red <- bar_transfer_red %>% add_trace(y = ~number20, name = '2020')
bar_transfer_red <- bar_transfer_red %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

bar_transfer_yellow <- plot_ly(av_m[which(av_m$StationType == "轉乘站" &av_m$MLineType == "環狀線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_transfer_yellow <- bar_transfer_yellow %>% add_trace(y = ~number20, name = '2020')
bar_transfer_yellow <- bar_transfer_yellow %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

bar_transfer_orange <- plot_ly(av_m[which(av_m$StationType == "轉乘站" &av_m$MLineType == "中和新蘆線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_transfer_orange <- bar_transfer_orange %>% add_trace(y = ~number20, name = '2020')
bar_transfer_orange <- bar_transfer_orange %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

bar_transfer_green <- plot_ly(av_m[which(av_m$StationType == "轉乘站" &av_m$MLineType == "松山新店線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_transfer_green <- bar_transfer_green %>% add_trace(y = ~number20, name = '2020')
bar_transfer_green <- bar_transfer_green %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

bar_transfer_brown <- plot_ly(av_m[which(av_m$StationType == "轉乘站" &av_m$MLineType == "文湖線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_transfer_brown <- bar_transfer_brown %>% add_trace(y = ~number20, name = '2020')
bar_transfer_brown <- bar_transfer_brown %>% layout(yaxis = list(title = 'Count'), barmode = 'group')


bar_brown_normal <- plot_ly(av_m[which(av_m$StationType == "一般車站" &av_m$MLineType == "文湖線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_brown_normal <- bar_brown_normal %>% add_trace(y = ~number20, name = '2020')
bar_brown_normal <- bar_brown_normal %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

bar_blue_normal <- plot_ly(av_m[which(av_m$StationType == "一般車站" &av_m$MLineType == "板南線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_blue_normal <- bar_blue_normal %>% add_trace(y = ~number20, name = '2020')
bar_blue_normal <- bar_blue_normal %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

bar_red_normal <- plot_ly(av_m[which(av_m$StationType == "一般車站" &av_m$MLineType == "淡水信義線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_red_normal <- bar_red_normal %>% add_trace(y = ~number20, name = '2020')
bar_red_normal <- bar_red_normal %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

bar_green_normal <- plot_ly(av_m[which(av_m$StationType == "一般車站" &av_m$MLineType == "松山新店線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_green_normal <- bar_green_normal %>% add_trace(y = ~number20, name = '2020')
bar_green_normal <- bar_green_normal %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

bar_yellow_normal <- plot_ly(av_m[which(av_m$StationType == "一般車站" &av_m$MLineType == "環狀線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_yellow_normal <- bar_yellow_normal %>% add_trace(y = ~number20, name = '2020')
bar_yellow_normal <- bar_yellow_normal %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

bar_orange_normal <- plot_ly(av_m[which(av_m$StationType == "一般車站" &av_m$MLineType == "中和新蘆線"),], x = ~station, y = ~number21, type = 'bar', name = '2021')
bar_orange_normal <- bar_orange_normal %>% add_trace(y = ~number20, name = '2020')
bar_orange_normal <- bar_orange_normal %>% layout(yaxis = list(title = 'Count'), barmode = 'group')

## percent  ##
pc_blue = plot_ly(av_m[which(av_m$MLineType == "板南線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')
pc_red = plot_ly(av_m[which(av_m$MLineType == "淡水信義線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')
pc_orange = plot_ly(av_m[which(av_m$MLineType == "中和新蘆線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')
pc_yellow = plot_ly(av_m[which(av_m$MLineType == "環狀線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')
pc_green = plot_ly(av_m[which(av_m$MLineType == "松山新店線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')
pc_brown = plot_ly(av_m[which(av_m$MLineType == "文湖線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')

pc_transfer_blue = plot_ly(av_m[which(av_m$StationType == "轉乘站"& av_m$MLineType == "板南線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')
pc_transfer_red = plot_ly(av_m[which(av_m$StationType == "轉乘站"& av_m$MLineType == "淡水信義線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')
pc_transfer_orange = plot_ly(av_m[which(av_m$StationType == "轉乘站"& av_m$MLineType == "中和新蘆線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')
pc_transfer_yellow = plot_ly(av_m[which(av_m$StationType == "轉乘站"& av_m$MLineType == "環狀線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')
pc_transfer_green = plot_ly(av_m[which(av_m$StationType == "轉乘站"& av_m$MLineType == "松山新店線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')
pc_transfer_brown = plot_ly(av_m[which(av_m$StationType == "轉乘站"& av_m$MLineType == "文湖線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')

pc_normal_blue = plot_ly(av_m[which(av_m$StationType == "一般車站"&av_m$MLineType == "板南線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')
pc_normal_red = plot_ly(av_m[which(av_m$StationType == "一般車站"&av_m$MLineType == "淡水信義線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')
pc_normal_orange = plot_ly(av_m[which(av_m$StationType == "一般車站"&av_m$MLineType == "中和新蘆線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')
pc_normal_yellow = plot_ly(av_m[which(av_m$StationType == "一般車站"&av_m$MLineType == "環狀線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')
pc_normal_green = plot_ly(av_m[which(av_m$StationType == "一般車站"&av_m$MLineType == "松山新店線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')
pc_normal_brown = plot_ly(av_m[which(av_m$StationType == "一般車站"&av_m$MLineType == "文湖線"),], x = ~station, y = ~percent, type = 'bar', name = 'ratio',text = ~percent, textposition = 'auto')

####### boxplot  #####
boxchart = plot_ly(avg, x = ~MLineType, y = ~person, type = "box",
                   color = ~year,
                   # boxpoints = "all",
                   # jitter = 0.01,
                   pointpos = 0,showlegend = T) %>%
  layout(boxmode = "group", height = 480)

####### combine  #####

b_blue <- subplot(bar_blue,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))

b_red <- subplot(bar_red,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))
b_orange <- subplot(bar_orange,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))
b_green <- subplot(bar_green,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))
b_yellow <- subplot(bar_yellow,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))
b_brown <- subplot(bar_brown,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))


b_blue_transfer <- subplot(bar_transfer_blue,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))

b_red_transfer <- subplot(bar_transfer_red,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))

b_yellow_transfer <- subplot(bar_transfer_yellow,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))

b_orange_transfer <- subplot(bar_transfer_orange,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))

b_green_transfer <- subplot(bar_transfer_green,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))

b_brown_transfer <- subplot(bar_transfer_brown,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))


b_blue_normal <- subplot(bar_blue_normal,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))

b_red_normal <- subplot(bar_red_normal,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))

b_yellow_normal <- subplot(bar_yellow_normal,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))

b_orange_normal <- subplot(bar_orange_normal,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))

b_green_normal <- subplot(bar_green_normal,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))

b_brown_normal <- subplot(bar_brown_normal,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "旅次量"), yaxis2 = list(title = "旅次量"))



p_blue <- subplot(pc_blue,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))
p_brown <- subplot(pc_brown,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))
p_red <- subplot(pc_red,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))
p_green <- subplot(pc_green,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))
p_orange <- subplot(pc_orange,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))
p_yellow <- subplot(pc_yellow,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))




p_blue_transfer <- subplot(pc_transfer_blue,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))
p_brown_transfer <- subplot(pc_transfer_brown,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))
p_red_transfer <- subplot(pc_transfer_red,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))
p_green_transfer <- subplot(pc_transfer_green,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))
p_orange_transfer <- subplot(pc_transfer_orange,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))
p_yellow_transfer <- subplot(pc_transfer_yellow,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))



p_blue_normal <- subplot(pc_normal_blue,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))
p_brown_normal <- subplot(pc_normal_brown,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))
p_red_normal <- subplot(pc_normal_red,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))
p_green_normal <- subplot(pc_normal_green,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))
p_orange_normal <- subplot(pc_normal_orange,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))
p_yellow_normal <- subplot(pc_normal_yellow,boxchart, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Station"), xaxis2 = list(title = "Lane"),
         yaxis = list(title = "比率"), yaxis2 = list(title = "旅次量"))





###################   panel3    ###########################
#######  oi      ######
pass_in_ <- pass_in %>% mutate(day = wday(Date))

pass_in_$weekday <- ifelse(pass_in_$day %in% c(6, 7), "weekend", "weekday")

pass_in_weekday_sum = colMeans(pass_in_[which(pass_in_$weekday == "weekday"),][,2:120])

pass_in_weekend_sum = colMeans(pass_in_[which(pass_in_$weekday == "weekend"),][,2:120])

pass_out_ <- pass_out %>% mutate(day = wday(Date))

pass_out_$weekday <- ifelse(pass_out_$day %in% c(6, 7), "weekend", "weekday")

pass_out_weekday_sum = colMeans(pass_out_[which(pass_out_$weekday == "weekday"),][,2:120])

pass_out_weekend_sum = colMeans(pass_out_[which(pass_out_$weekday == "weekend"),][,2:120])

day_end = data.frame(
  station = colnames(sum2020)[2:120],
  全時段進站人次 = pass_in_weekday_sum+pass_in_weekend_sum,
  全時段出站人次 = pass_out_weekday_sum+pass_out_weekend_sum,
  ratio_oi = (pass_out_weekday_sum+pass_out_weekend_sum)/(pass_in_weekday_sum+pass_in_weekend_sum),
  平日進站人次 = pass_in_weekday_sum,
  假日進站人次 = pass_in_weekend_sum,
  平日出站人次 = pass_out_weekday_sum,
  假日出站人次 = pass_out_weekend_sum,
  ratio_week_oi = pass_out_weekday_sum/pass_in_weekday_sum,
  ratio_end_oi = pass_out_weekend_sum/pass_in_weekend_sum
)

all_oi = ggplot(day_end,aes(x = 全時段進站人次, y = 全時段出站人次, color = station))+geom_point()+geom_abline(intercept = 0, slope = 1, color="grey",linetype = "dashed", size=0.5)+theme_bw()+xlab("進站人次")+ylab("出站人次")
all_oi = ggplotly(all_oi)

week_oi = ggplot(day_end,aes(x = 平日進站人次, y = 平日出站人次, color = station))+geom_point()+geom_abline(intercept = 0, slope = 1, color="grey",linetype = "dashed", size=0.5)+theme_bw()+xlab("進站人次")+ylab("出站人次")
week_oi = ggplotly(week_oi)

end_oi = ggplot(day_end,aes(x = 假日進站人次, y = 假日出站人次, color = station))+geom_point()+geom_abline(intercept = 0, slope = 1, color="grey",linetype = "dashed", size=0.5)+theme_bw()+xlab("進站人次")+ylab("出站人次")
end_oi = ggplotly(end_oi)
######  trend    #####

blue = spot[which(spot$MLineType == "板南線"),]
brown = spot[which(spot$MLineType == "文湖線"),]
red = spot[which(spot$MLineType == "淡水信義線"),]
orange = spot[which(spot$MLineType == "中和新蘆線"),]
yellow = spot[which(spot$MLineType == "環狀線"),]
green = spot[which(spot$MLineType == "松山新店線"),]

blue_index = which(colnames(sums) %in% blue$StationName__Zh_tw)
brown_index = which(colnames(sums) %in% brown$StationName__Zh_tw)
red_index = which(colnames(sums) %in% red$StationName__Zh_tw)
orange_index = which(colnames(sums) %in% orange$StationName__Zh_tw)
yellow_index = which(colnames(sums) %in% yellow$StationName__Zh_tw)
green_index = which(colnames(sums) %in% green$StationName__Zh_tw)

lane = data.frame(Date = sums$Date,
                  板南線 = rowSums(sums[,blue_index]),
                  文湖線 = rowSums(sums[,brown_index]),
                  淡水信義線 = rowSums(sums[,red_index]),
                  中和新蘆線 = rowSums(sums[,orange_index]),
                  環狀線 = rowSums(sums[,yellow_index]),
                  松山新店線 = rowSums(sums[,green_index]))

#####   year trend
## all
lane$months = month(lane$Date)
lane$wday = wday(lane$Date, week_start = 1)
lane.xts = xts(lane[,2:9], order.by = lane$Date)
lane.month.xts = apply.monthly(lane.xts, 
                               function(x)
                                 colSums(x, na.rm = T))
a1_all = ts_plot(lane.month.xts[,1:6], Xtitle = "Date", Ytitle = "人流量", title = "")
## single
lane_blue.xts = xts(lane[,c(2,8,9)], order.by = lane$Date)
lane_blue.month.xts = apply.monthly(lane_blue.xts, 
                                    function(x)
                                      colSums(x, na.rm = T))
a1_blue = ts_plot(lane_blue.month.xts[,1], Xtitle = "Date", Ytitle = "人流量", title = "")

lane_brown.xts = xts(lane[,c(3,8,9)], order.by = lane$Date)
lane_brown.month.xts = apply.monthly(lane_brown.xts, 
                                    function(x)
                                      colSums(x, na.rm = T))
a1_brown = ts_plot(lane_brown.month.xts[,1], Xtitle = "Date", Ytitle = "人流量", title = "")

lane_red.xts = xts(lane[,c(4,8,9)], order.by = lane$Date)
lane_red.month.xts = apply.monthly(lane_red.xts, 
                                    function(x)
                                      colSums(x, na.rm = T))
a1_red = ts_plot(lane_red.month.xts[,1], Xtitle = "Date", Ytitle = "人流量", title = "")

lane_orange.xts = xts(lane[,c(5,8,9)], order.by = lane$Date)
lane_orange.month.xts = apply.monthly(lane_orange.xts, 
                                    function(x)
                                      colSums(x, na.rm = T))
a1_orange = ts_plot(lane_orange.month.xts[,1], Xtitle = "Date", Ytitle = "人流量", title = "")

lane_yellow.xts = xts(lane[,c(6,8,9)], order.by = lane$Date)
lane_yellow.month.xts = apply.monthly(lane_yellow.xts, 
                                    function(x)
                                      colSums(x, na.rm = T))
a1_yellow = ts_plot(lane_yellow.month.xts[,1], Xtitle = "Date", Ytitle = "人流量", title = "")
lane_green.xts = xts(lane[,c(7,8,9)], order.by = lane$Date)
lane_green.month.xts = apply.monthly(lane_green.xts, 
                                    function(x)
                                      colSums(x, na.rm = T))
a1_green = ts_plot(lane_green.month.xts[,1], Xtitle = "Date", Ytitle = "人流量", title = "")

#####   month trend
## all
lane.split = split(lane.xts, f = "years")
lane.lapply = lapply(lane.split, function(x) x %>%
                       as.data.frame() %>%
                       group_by(months) %>%
                       summarise(Count =sum(板南線,淡水信義線,松山新店線,中和新蘆線,文湖線,環狀線, na.rm = T)) %>%
                       pull(Count))
lane.cbind = do.call(cbind, lane.lapply)
lane.cbind[12,2] = NA
lane_s.ts = ts(lane.cbind %>% as.numeric(), start = 2020, frequency = 12)
a2_all = ts_seasonal(lane_s.ts, title = "")

## single

lane_blue.split = split(lane_blue.xts, f = "years")
lane_blue.lapply = lapply(lane_blue.split, function(x) x %>%
                            as.data.frame() %>%
                            group_by(months) %>%
                            summarise(Count = 
                                        sum(板南線, na.rm = T)) %>%
                            pull(Count))
lane_blue.cbind = do.call(cbind, lane_blue.lapply)
lane_blue.cbind[12,2] = NA
lane_blue.ts = ts(lane_blue.cbind %>% as.numeric(), start = 2020, frequency = 12)
a2_blue = ts_seasonal(lane_blue.ts, title = "")

lane_brown.split = split(lane_brown.xts, f = "years")
lane_brown.lapply = lapply(lane_brown.split, function(x) x %>%
                            as.data.frame() %>%
                            group_by(months) %>%
                            summarise(Count = 
                                        sum(文湖線, na.rm = T)) %>%
                            pull(Count))
lane_brown.cbind = do.call(cbind, lane_brown.lapply)
lane_brown.cbind[12,2] = NA
lane_brown.ts = ts(lane_brown.cbind %>% as.numeric(), start = 2020, frequency = 12)
a2_brown = ts_seasonal(lane_brown.ts, title = "")

lane_red.split = split(lane_red.xts, f = "years")
lane_red.lapply = lapply(lane_red.split, function(x) x %>%
                             as.data.frame() %>%
                             group_by(months) %>%
                             summarise(Count = 
                                         sum(淡水信義線, na.rm = T)) %>%
                             pull(Count))
lane_red.cbind = do.call(cbind, lane_red.lapply)
lane_red.cbind[12,2] = NA
lane_red.ts = ts(lane_red.cbind %>% as.numeric(), start = 2020, frequency = 12)
a2_red = ts_seasonal(lane_red.ts, title = "")

lane_orange.split = split(lane_orange.xts, f = "years")
lane_orange.lapply = lapply(lane_orange.split, function(x) x %>%
                             as.data.frame() %>%
                             group_by(months) %>%
                             summarise(Count = 
                                         sum(中和新蘆線, na.rm = T)) %>%
                             pull(Count))
lane_orange.cbind = do.call(cbind, lane_orange.lapply)
lane_orange.cbind[12,2] = NA
lane_orange.ts = ts(lane_orange.cbind %>% as.numeric(), start = 2020, frequency = 12)
a2_orange = ts_seasonal(lane_orange.ts, title = "")

lane_yellow.split = split(lane_yellow.xts, f = "years")
lane_yellow.lapply = lapply(lane_yellow.split, function(x) x %>%
                             as.data.frame() %>%
                             group_by(months) %>%
                             summarise(Count = 
                                         sum(環狀線, na.rm = T)) %>%
                             pull(Count))
lane_yellow.cbind = do.call(cbind, lane_yellow.lapply)
lane_yellow.cbind[12,2] = NA
lane_yellow.ts = ts(lane_yellow.cbind %>% as.numeric(), start = 2020, frequency = 12)
a2_yellow = ts_seasonal(lane_yellow.ts, title = "")

lane_green.split = split(lane_green.xts, f = "years")
lane_green.lapply = lapply(lane_green.split, function(x) x %>%
                             as.data.frame() %>%
                             group_by(months) %>%
                             summarise(Count = 
                                         sum(松山新店線, na.rm = T)) %>%
                             pull(Count))
lane_green.cbind = do.call(cbind, lane_green.lapply)
lane_green.cbind[12,2] = NA
lane_green.ts = ts(lane_green.cbind %>% as.numeric(), start = 2020, frequency = 12)
a2_green = ts_seasonal(lane_green.ts, title = "")
### heatmap
sums.xts = xts(sums[,2:119], order.by = sums$Date)

#長期趨勢
sums.month.xts = apply.monthly(sums.xts, 
                               function(x)
                                 colSums(x, na.rm = T))

sums.month.df =data.frame(index(sums.month.xts),
                          sums.month.xts)
colnames(sums.month.df)[1] = "Date"



#heatmap
sums$sums = rowSums(sums[,2:119])
sums$months = month(sums$Date)
sums$wday = wday(sums$Date, week_start = 1)

sums_ = subset(sums,select=c(Date,sums,months,wday))

sums_.xts = xts(sums_[, 2:4], order.by = sums_$Date)
sums_.split = split(sums_.xts, f = "years")
sums_.lapply = lapply(sums_.split, function(x) x %>%
                        as.data.frame() %>%
                        group_by(months) %>%
                        summarise(Count =
                                    sum(sums, na.rm = T)) %>%
                        pull(Count))

sums_.cbind = do.call(cbind, sums_.lapply)
sums_.cbind[12,2] = NA
sums_h.ts = ts(sums_.cbind %>% as.numeric(), start = 2020, frequency = 12)
sums_s.ts = ts(sums_.cbind %>% as.numeric(), start = 2020, frequency = 12)
a_year = ts_heatmap(sums_h.ts, title = "")
# ts_seasonal(sums_s.ts, title = "")



sums_.xts = xts(sums_[, 2:4], order.by = sums_$Date)
sums_.split = split(sums_.xts, f = "months")
sums_.lapply = lapply(sums_.split, function(x) x %>%
                        as.data.frame() %>%
                        group_by(wday) %>%
                        summarise(Count =
                                    sum(sums, na.rm = T)) %>%
                        pull(Count))

sums_.cbind = do.call(cbind, sums_.lapply)
sums_.ts = ts(sums_.cbind %>% as.numeric(), start = 1,frequency = 7)

a_month = ts_heatmap(sums_.ts, title = "") %>% layout(xaxis = list(title = "month"))
### combine

a1_all_ <- subplot(a1_all,a_year, nrows = 2,margin = 0.15) %>%
  layout(xaxis = list(title = "Time"), xaxis2 = list(title = "Year"),
         yaxis = list(title = "Passengers"), yaxis2 = list(title = "Month"))
a1_blue_ <- subplot(a1_blue,a_year, nrows = 2,margin = 0.15) %>%
  layout(xaxis = list(title = "Time"), xaxis2 = list(title = "Year"),
         yaxis = list(title = "Passengers"), yaxis2 = list(title = "Month"))
a1_red_ <- subplot(a1_red,a_year, nrows = 2,margin = 0.15) %>%
  layout(xaxis = list(title = "Time"), xaxis2 = list(title = "Year"),
         yaxis = list(title = "Passengers"), yaxis2 = list(title = "Month"))
a1_yellow_ <- subplot(a1_yellow,a_year, nrows = 2,margin = 0.15) %>%
  layout(xaxis = list(title = "Time"), xaxis2 = list(title = "Year"),
         yaxis = list(title = "Passengers"), yaxis2 = list(title = "Month"))
a1_orange_ <- subplot(a1_orange,a_year, nrows = 2,margin = 0.15) %>%
  layout(xaxis = list(title = "Time"), xaxis2 = list(title = "Year"),
         yaxis = list(title = "Passengers"), yaxis2 = list(title = "Month"))
a1_green_ <- subplot(a1_green,a_year, nrows = 2,margin = 0.15) %>%
  layout(xaxis = list(title = "Time"), xaxis2 = list(title = "Year"),
         yaxis = list(title = "Passengers"), yaxis2 = list(title = "Month"))
a1_brown_ <- subplot(a1_brown,a_year, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Time"), xaxis2 = list(title = "Year"),
         yaxis = list(title = "Passengers"), yaxis2 = list(title = "Month"))


a2_all_ <- subplot(a2_all,a_month, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Month"), xaxis2 = list(title = "Month"),
         yaxis = list(title = "Passengers"), yaxis2 = list(title = "Day"))
a2_blue_ <- subplot(a2_blue,a_month, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Month"), xaxis2 = list(title = "Month"),
         yaxis = list(title = "Passengers"), yaxis2 = list(title = "Day"))
a2_red_ <- subplot(a2_red,a_month, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Month"), xaxis2 = list(title = "Month"),
         yaxis = list(title = "Passengers"), yaxis2 = list(title = "Day"))
a2_yellow_ <- subplot(a2_yellow,a_month, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Month"), xaxis2 = list(title = "Month"),
         yaxis = list(title = "Passengers"), yaxis2 = list(title = "Day"))
a2_orange_ <- subplot(a2_orange,a_month, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Month"), xaxis2 = list(title = "Month"),
         yaxis = list(title = "Passengers"), yaxis2 = list(title = "Day"))
a2_green_ <- subplot(a2_green,a_month, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Month"), xaxis2 = list(title = "Month"),
         yaxis = list(title = "Passengers"), yaxis2 = list(title = "Day"))
a2_brown_ <- subplot(a2_brown,a_month, nrows = 2,margin = 0.12) %>%
  layout(xaxis = list(title = "Month"), xaxis2 = list(title = "Month"),
         yaxis = list(title = "Passengers"), yaxis2 = list(title = "Day"))







###########################################################
shinyServer(function(input, output,session) {
  ###### panel 1 ########
  output$map <- renderLeaflet({
    if(input$radiobutton == "所有路線"){
      map_all@map %>% setView(lng = 121.5, lat = 25.06, zoom = 11) 
    }
    else if(input$radiobutton == "板南線"){
      map_blue@map %>% setView(lng = 121.5, lat = 25.06, zoom = 11) 
    }
    else if(input$radiobutton == "松山新店線"){
      (map_green+map_lgreen)@map %>% setView(lng = 121.5, lat = 25.06, zoom = 11)
    }
    else if(input$radiobutton == "淡水信義線"){
      (map_red+map_lred)@map %>% setView(lng = 121.5, lat = 25.06, zoom = 11) 
    }
    else if(input$radiobutton == "文湖線"){
      map_brown@map %>% setView(lng = 121.5, lat = 25.06, zoom = 11) 
    }
    else if(input$radiobutton == "中和新蘆線"){
      map_orange@map %>% setView(lng = 121.5, lat = 25.06, zoom = 11) 
    }
    else if(input$radiobutton == "環狀線"){
      map_yellow@map %>% setView(lng = 121.5, lat = 25.06, zoom = 11) 
    }
    
  })
  output$basic <- renderPlotly({
    if(input$xaxis == "營運車站數" & input$yaxis == "營運車站數"){
      chart11
    }
    else if(input$xaxis == "營運車站數" & input$yaxis == "營運路線數"){
      chart12
    }
    else if(input$xaxis == "營運車站數" & input$yaxis == "營運里程"){
      chart13
    }
    else if(input$xaxis == "營運車站數" & input$yaxis == "客運人次"){
      chart14
    }
    else if(input$xaxis == "營運車站數" & input$yaxis == "客運收入千元"){
      chart15
    }
    
    
    else if(input$xaxis == "營運路線數" & input$yaxis == "營運車站數"){
      chart21
    }
    else if(input$xaxis == "營運路線數" & input$yaxis == "營運路線數"){
      chart22
    }
    else if(input$xaxis == "營運路線數" & input$yaxis == "營運里程"){
      chart23
    }
    else if(input$xaxis == "營運路線數" & input$yaxis == "客運人次"){
      chart24
    }
    else if(input$xaxis == "營運路線數" & input$yaxis == "客運收入千元"){
      chart25
    }
    
    else if(input$xaxis == "營運里程" & input$yaxis == "營運車站數"){
      chart31
    }
    else if(input$xaxis == "營運里程" & input$yaxis == "營運路線數"){
      chart32
    }
    else if(input$xaxis == "營運里程" & input$yaxis == "營運里程"){
      chart33
    }
    else if(input$xaxis == "營運里程" & input$yaxis == "客運人次"){
      chart34
    }
    else if(input$xaxis == "營運里程" & input$yaxis == "客運收入千元"){
      chart35
    }
    
    else if(input$xaxis == "客運人次" & input$yaxis == "營運車站數"){
      chart41
    }
    else if(input$xaxis == "客運人次" & input$yaxis == "營運路線數"){
      chart42
    }
    else if(input$xaxis == "客運人次" & input$yaxis == "營運里程"){
      chart43
    }
    else if(input$xaxis == "客運人次" & input$yaxis == "客運人次"){
      chart44
    }
    else if(input$xaxis == "客運人次" & input$yaxis == "客運收入千元"){
      chart45
    }
    
    else if(input$xaxis == "客運收入千元" & input$yaxis == "營運車站數"){
      chart51
    }
    else if(input$xaxis == "客運收入千元" & input$yaxis == "營運路線數"){
      chart52
    }
    else if(input$xaxis == "客運收入千元" & input$yaxis == "營運里程"){
      chart53
    }
    else if(input$xaxis == "客運收入千元" & input$yaxis == "客運人次"){
      chart54
    }
    else if(input$xaxis == "客運收入千元" & input$yaxis == "客運收入千元"){
      chart55
    }
  })
  ###### panel 2 ###########
  output$number <- renderPlotly({
    if(input$lane == "板南線" &  input$stype == "全部車站" & input$change == "旅次量"){
      b_blue
    }
    else if(input$lane == "環狀線" &  input$stype == "全部車站" & input$change == "旅次量"){
      b_yellow
    }
    else if(input$lane == "淡水信義線" &  input$stype == "全部車站" & input$change == "旅次量"){
      b_red
    }
    else if(input$lane == "中和新蘆線" &  input$stype == "全部車站" & input$change == "旅次量"){
      b_orange
    }
    else if(input$lane == "松山新店線" &  input$stype == "全部車站" & input$change == "旅次量"){
      b_green
    }
    else if(input$lane == "文湖線" &  input$stype == "全部車站" & input$change == "旅次量"){
      b_brown
    }    
    
    
    else if(input$lane == "板南線" &  input$stype == "轉乘站" & input$change == "旅次量"){
      b_blue_transfer
    }
    else if(input$lane == "淡水信義線" &  input$stype == "轉乘站" & input$change == "旅次量"){
      b_red_transfer
    }
    else if(input$lane == "環狀線" &  input$stype == "轉乘站" & input$change == "旅次量"){
      b_yellow_transfer
    }
    else if(input$lane == "松山新店線" &  input$stype == "轉乘站" & input$change == "旅次量"){
      b_green_transfer
    }
    else if(input$lane == "文湖線" &  input$stype == "轉乘站" & input$change == "旅次量"){
      b_brown_transfer
    }
    else if(input$lane == "中和新蘆線" &  input$stype == "轉乘站" & input$change == "旅次量"){
      b_orange_transfer
    }
    
    
    else if(input$lane == "中和新蘆線" &  input$stype == "一般車站" & input$change == "旅次量"){
      b_orange_normal
    }
    else if(input$lane == "環狀線" &  input$stype == "一般車站" & input$change == "旅次量"){
      b_yellow_normal
    }
    else if(input$lane == "板南線" &  input$stype == "一般車站" & input$change == "旅次量"){
      b_blue_normal
    }
    else if(input$lane == "松山新店線" &  input$stype == "一般車站" & input$change == "旅次量"){
      b_green_normal
    }
    else if(input$lane == "淡水信義線" &  input$stype == "一般車站" & input$change == "旅次量"){
      b_red_normal
    }
    else if(input$lane == "文湖線" &  input$stype == "一般車站" & input$change == "旅次量"){
      b_brown_normal
    }
    
    
    else if(input$lane == "中和新蘆線" &  input$stype == "全部車站" & input$change == "百分比"){
      p_orange
    }
    else if(input$lane == "環狀線" &  input$stype == "全部車站" & input$change == "百分比"){
      p_yellow
    }
    else if(input$lane == "板南線" &  input$stype == "全部車站" & input$change == "百分比"){
      p_blue
    }
    else if(input$lane == "松山新店線" &  input$stype == "全部車站" & input$change == "百分比"){
      p_green
    }
    else if(input$lane == "淡水信義線" &  input$stype == "全部車站" & input$change == "百分比"){
      p_red
    }
    else if(input$lane == "文湖線" &  input$stype == "全部車站" & input$change == "百分比"){
      p_brown
    }
    
    
    else if(input$lane == "中和新蘆線" &  input$stype == "一般車站" & input$change == "百分比"){
      p_orange_normal
    }
    else if(input$lane == "環狀線" &  input$stype == "一般車站" & input$change == "百分比"){
      p_yellow_normal
    }
    else if(input$lane == "板南線" &  input$stype == "一般車站" & input$change == "百分比"){
      p_blue_normal
    }
    else if(input$lane == "松山新店線" &  input$stype == "一般車站" & input$change == "百分比"){
      p_green_normal
    }
    else if(input$lane == "淡水信義線" &  input$stype == "一般車站" & input$change == "百分比"){
      p_red_normal
    }
    else if(input$lane == "文湖線" &  input$stype == "一般車站" & input$change == "百分比"){
      p_brown_normal
    }
    
    
    else if(input$lane == "中和新蘆線" &  input$stype == "轉乘站" & input$change == "百分比"){
      p_orange_transfer
    }
    else if(input$lane == "環狀線" &  input$stype == "轉乘站" & input$change == "百分比"){
      p_yellow_transfer
    }
    else if(input$lane == "板南線" &  input$stype == "轉乘站" & input$change == "百分比"){
      p_blue_transfer
    }
    else if(input$lane == "松山新店線" &  input$stype == "轉乘站" & input$change == "百分比"){
      p_green_transfer
    }
    else if(input$lane == "淡水信義線" &  input$stype == "轉乘站" & input$change == "百分比"){
      p_red_transfer
    }
    else if(input$lane == "文湖線" &  input$stype == "轉乘站" & input$change == "百分比"){
      p_brown_transfer
    }
    
  })
  ###### panel 3 ########
  output$trending <- renderPlotly({
    if(input$line == "所有路線" & input$trend == "年趨勢"){
      a1_all_
    }
    else if(input$line == "板南線" & input$trend == "年趨勢"){
      a1_blue_
    }
    else if(input$line == "淡水信義線" & input$trend == "年趨勢"){
      a1_red_
    }
    else if(input$line == "中和新蘆線" & input$trend == "年趨勢"){
      a1_orange_
    }
    else if(input$line == "松山新店線" & input$trend == "年趨勢"){
      a1_green_
    }
    else if(input$line == "文湖線" & input$trend == "年趨勢"){
      a1_brown_
    }
    else if(input$line == "環狀線" & input$trend == "年趨勢"){
      a1_yellow_
    }
    
    
    else if(input$line == "所有路線" & input$trend == "月趨勢"){
      a2_all_
    }
    else if(input$line == "板南線" & input$trend == "月趨勢"){
      a2_blue_
    }
    else if(input$line == "淡水信義線" & input$trend == "月趨勢"){
      a2_red_
    }
    else if(input$line == "中和新蘆線" & input$trend == "月趨勢"){
      a2_orange_
    }
    else if(input$line == "環狀線" & input$trend == "月趨勢"){
      a2_yellow_
    }
    else if(input$line == "文湖線" & input$trend == "月趨勢"){
      a2_brown_
    }
    else if(input$line == "松山新店線" & input$trend == "月趨勢"){
      a2_green_
    }
    
  })
  output$oi <- renderPlotly({
    if(input$week == "全時段"){
      all_oi
    }
    else if(input$week =="平日"){
      week_oi
    }
    else if(input$week =="假日"){
      end_oi
    }
  })
})
