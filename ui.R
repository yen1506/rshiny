library(shiny)
# library(ggplot2)
# library(leaflet)
# library(plotly)
library(shinydashboard)
# library(shinyWidgets)
# library(sf)
# library(tmap)
# library(tidyverse)
# library(reshape2)
library(ggplot2)
# library(showtext)
# library(cartography)
# library(mapview)
# library(systemfonts)
# library(timeDate)
library(leaflet)
# library(leafpop)
library(shiny)
# library(rlist)
# library(showtext)
# library(sysfonts)
# library(tidyr)
# library(showtextdb)
# library(xts)
# library(TSstudio)
# library(lubridate)
library(plotly)
# library(magrittr)
# library(directlabels)

ui <- dashboardPage(
    dashboardHeader(title = "Taipei Metro"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("車站地圖和基本資料",tabName = "dashboard",icon = icon("map-marked-alt")),
            menuItem("運量統計",tabName = "statistic",icon = icon("calculator")),
            menuItem("資料分析",tabName = "analysis",icon = icon("bar-chart"))
        )),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                    fluidRow(
                        infoBox("營運公里數",146.2,icon = icon("ruler")),
                        infoBox("營運車站數",131,icon = icon("subway")),
                        infoBox("服務人次","over 10 Billions",icon = icon("user-friends"))
                    ),
                    fluidRow(
                       box(width = 4,
                           height = 575,
                           h4("車站地圖"),
                           selectInput("radiobutton", label = h5("捷運線（含支線）"),
                                       choices = c("所有路線","板南線","淡水信義線","環狀線",
                                                   "文湖線","松山新店線","中和新蘆線"),
                                       selected = "所有路線"),
                           hr(),
                           h4("基本資料"),
                           selectInput("xaxis", label = h5("x軸"),
                                       choices = c("營運車站數","營運路線數","營運里程",
                                                   "客運人次","客運收入千元"),
                                       selected = "營運車站數"),
                           selectInput("yaxis", label = h5("y軸"),
                                       choices = c("營運車站數","營運路線數","營運里程",
                                                   "客運人次","客運收入千元"),
                                       selected = "營運路線數")
                       ),
                       tabBox(width = 8,
                              height = 530,
                              selected = "車站地圖",
                              tabPanel("車站地圖",
                                       leafletOutput("map", height = 510)),
                              tabPanel("基本資料",
                                       plotlyOutput("basic",height = 510))
                       )

                    ),
                    
            ),
            
            tabItem(tabName = "statistic",
                    fluidRow(
                        infoBox("營運公里數",146.2,icon = icon("ruler")),
                        infoBox("營運車站數",131,icon = icon("subway")),
                        infoBox("服務人次","over 10 Billions",icon = icon("user-friends"))
                    ),
                    fluidRow(
                        box(width = 4,
                            height = 575,
                            selectInput("lane", label = h5("捷運線（含支線）"),
                                        choices = c("板南線","淡水信義線","環狀線",
                                                    "文湖線","松山新店線","中和新蘆線"),
                                        selected = "板南線"),
                            radioButtons("stype", label = h5("站點性質"),
                                         choices = c("全部車站","轉乘站","一般車站"),
                                         selected = "全部車站"),
                            radioButtons("change", label = h5("年度變化"),
                                         choices = c("旅次量","百分比"),
                                         selected = "旅次量"),
                            
                        ),
                        box(width = 8,height = 575,
                            plotlyOutput("number",height = 575)
                        )
                    ),
                    
            ),
            
            
            
            tabItem(tabName = "analysis",
                    fluidRow(
                        infoBox("營運公里數",146.2,icon = icon("ruler")),
                        infoBox("營運車站數",131,icon = icon("subway")),
                        infoBox("服務人次","over 10 Billions",icon = icon("user-friends"))
                    ),
                    fluidRow(
                        box(width = 4,
                            height = 575,
                            h4("趨勢分析"),
                            selectInput("line", label = h5("捷運線（含支線）"),
                                        choices = c("所有路線","板南線","淡水信義線","環狀線",
                                                    "文湖線","松山新店線","中和新蘆線"),
                                        selected = "所有路線"),
                            radioButtons("trend", label = h5("趨勢比較"),
                                         choices = c("年趨勢","月趨勢"),
                                         selected = "年趨勢"),
                            hr(),
                            h4("進出站分析"),
                            radioButtons("week", label = h5("進出站人次"),
                                        choices = c("全時段","平日","假日"),
                                        selected = "全時段")
                        ),
                        tabBox(width = 8,
                               height = 575,
                               selected = "趨勢分析",
                               tabPanel("趨勢分析",
                                        plotlyOutput("trending"),height = 575),
                               tabPanel("進出站分析",
                                        plotlyOutput("oi"),height = 575)
                        )
                        
                    )
            )
        )
    )
)
