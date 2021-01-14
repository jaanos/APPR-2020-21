library(shiny)

fluidPage(
  titlePanel(""),
  
  tabPanel("Graf",
           sidebarPanel(
             selectInput("Leto", label = "Izberi leto", 
                         choices = unique(vrste_dohodka$Leto)),
             checkboxGroupInput("Vrsta", "Izbere vrste:",
                                c("DOHODEK IZ DELA", "POKOJNINE Z DODATKI",
                                  "DRUŽINSKI IN SOCIALNI PREJEMKI", "DRUGI DOHODKI")),),
           mainPanel(plotOutput("graf_vrste")))
  
  
  
)
