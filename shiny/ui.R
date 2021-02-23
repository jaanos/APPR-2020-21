
shinyUI(fluidPage(
  theme=shinytheme("lumen"),
  headerPanel("Število prihodov po posameznih občinah"),
  fluidRow(sidebarLayout(sidebarPanel(selectInput("izbira",
              label=strong("Izbira leta:"), 
              choices=c("2018","2019"),
              selected="2019"),
              p("1.kvartil: 0-249 prihodov"),
              p("2.kvartil: 250-2297 prihodov"),
              p("3.kvartil: 2298-13261 prihodov"),
              p("2.kvartil: 13262-1022862 prihodov")),
              mainPanel(plotOutput("map"))))))



