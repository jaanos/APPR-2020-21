library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("slate"),
       navbarPage("Analiza plač", 
        tabPanel("Histogram",
        titlePanel(title=h2("Analiza glede na povprečno urno postavko", align="center")),
        sidebarLayout(
        sidebarPanel(
                      sliderInput(inputId = "bini",
                                  label = "Število binov:",
                                  min = 0,
                                  max = 80,
                                  value = 10
                                  )
                    ), #side panel
                    mainPanel(
                      plotOutput(outputId = "distPlot")
                    ) # main panel
                  )), # tab panel & sidbar layout
          tabPanel("Analiza po državah",
          titlePanel(title=h2("Analiza plač glede na državo", align="center")),
          sidebarLayout(
          sidebarPanel(
                                sliderInput(inputId = "leto",
                                            label = "Leto:",
                                            min=2006,
                                            max=2018,
                                            value=2010,
                                            step=2),
                                selectInput(inputId = "drzava",
                                            label = "Država:",
                                            choices = c(sort(unique(t_e_s$STATE)))
                                )
                   ),
                        mainPanel(
                                plotOutput(outputId = "distPlot2")
                        )
                   )), # 2. tab 
          tabPanel("Nekaj tretjega") # 3. tab
          ) # NAV
        )) #shiny in fluid         
                  
                  
                
     
