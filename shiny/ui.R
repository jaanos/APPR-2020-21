
shinyUI(fluidPage(
  theme=shinytheme("lumen"),
  headerPanel("Število prihodov po posameznih občinah"),
  fluidRow(sidebarLayout(sidebarPanel(selectInput("izbira",
              label=strong("Izbira leta:"), 
              choices=c("2018","2019"),
              selected="2018"),
              p("1.skupina: 0-249 prihodov"),
              p("2.skupina: 250-2297 prihodov"),
              p("3.skupina: 2298-13261 prihodov"),
              p("4.skupina: 13262-1022862 prihodov")),
              mainPanel(plotOutput("map"))))))



