library(shiny)
library(dplyr)

shinyUI(fluidPage(
  
  titlePanel("Analiza priseljevanja in izseljevanja v Sloveniji"),
  
  tabsetPanel(
    tabPanel("Tabela priseljevanja po regijah",
             DT::dataTableOutput("tabela")),
    
    
    tabPanel("Priseljevanje v Slovenijo po državah",
             sidebarPanel(
               
               selectInput("drzava", label = "Izberite državo:",
                           choices=(sort(unique(shiny_tab2$Drzava))))),
             
             mainPanel(plotOutput("priseljevanje"))),
    
    
    tabPanel("Priseljevanje v Slovenijo glede na izobrazbo",
             sidebarPanel(
               
               selectInput("izob", label = "Izberite stopnjo izobrazbe:",
                           choices=(sort(unique(shiny_tab1$Izobrazba))))),
             
             mainPanel(plotOutput("izobrazba"))),
    
    
    
    tabPanel("Priseljevanje v Slovenijo glede na dejavnost",
             sidebarPanel(
               
               selectInput("dej", label = "Izberite dejavnost:",
                           choices=(sort(unique(shiny_tab3$Dejavnost))))),
             
             mainPanel(plotOutput("dejavnost"))),
    
    tabPanel("Struktura priseljevanja v Slovenijo glede na leto in spol",
             sidebarPanel(
               
               selectInput("leto", label = "Izberite leto:",
                           choices=(sort(unique(tabela1$Leto)))),
               
               selectInput("spol", label = "Izberite spol:",
                           choices=(sort(unique(tabela1$Spol))))),
             
             mainPanel(plotOutput("struktura"))),
    
    
    tabPanel("Izseljevanje v Slovenijo glede na izobrazbo",
             sidebarPanel(
               
               selectInput("izob2", label = "Izberite stopnjo izobrazbe:",
                           choices=(sort(unique(shiny_tab4$Izobrazba))))),
             
             mainPanel(plotOutput("izobrazba2"))),
    
    tabPanel("Izseljevanje v Slovenijo glede na dejavnost",
             sidebarPanel(
               
               selectInput("dejiz", label = "Izberite dejavnost:",
                           choices=(sort(unique(shiny_tab5$Dejavnost))))),
             
             mainPanel(plotOutput("dejavnostiz"))),
    
    

    
    uiOutput("izborTabPanel")))
  
  
  
)
