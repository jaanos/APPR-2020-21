library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Analiza glasovanja na tekmovanju za Pesem Evrovizije"),
  
  tabsetPanel(
    tabPanel("Prva tabela",
             DT::dataTableOutput("tabela")),
    
    
    tabPanel("Rezultati za Jugoslavijo",
             sidebarPanel(
               
               selectInput("drzava", label = "Izberite državo:",
                           choices=(sort(unique(tabela3$Drzava))))),
             
             mainPanel(plotOutput("tocke"))),
    
    uiOutput("izborTabPanel")))
  
  
  
)