# library(shiny)
# 
# shinyUI(fluidPage(
#   
#   titlePanel("Slovenske občine"),
#   
#   tabsetPanel(
#       tabPanel("Velikost družine",
#                DT::dataTableOutput("druzine")),
#       
#       tabPanel("Število naselij",
#                sidebarPanel(
#                   uiOutput("pokrajine")
#                 ),
#                mainPanel(plotOutput("naselja")))
#     )
# ))


library(shiny)

ui <- fluidPage(
  titlePanel("Primerjava izbranih statistik na 90 odigranih minut"),
  inputPanel(
    selectInput('x', 'Prva Statistika:', choices = names(kom19)[c(-21,-36)][7:34], selected = "goals_scored"),
    selectInput('y', 'Druga Statistika:', choices = names(kom19)[c(-21,-36)][7:34], selected = "assists"),
    selectInput("r", "Pozicija:", choices = c("Vse", levels(kom19$Pozicija)), selected = "Vse"),
    sliderInput("z", "Kvantil Prve Statistike", min = 0.5, max = 0.95, value = 0.7),
    sliderInput("w", "Kvantil Druge Statistike", min = 0.5, max = 0.95, value = 0.7),
    sliderInput("q", " Minimalno število odigranih tekem", min=1, max = 30, value = 10)),
  mainPanel(plotlyOutput("graf"))
  ) 





# names(p19)[8:ncol(p19)]
