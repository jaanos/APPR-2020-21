library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("slate"),
       navbarPage("Analiza plač",
        tabPanel("Histogram",
        titlePanel("Analiza glede na povprečno urno postavko"),
        sidebarLayout(
        sidebarPanel(
                      sliderInput(inputId = "bini",
                                  label = "Število binov:",
                                  min = 0,
                                  max = 20,
                                  value = 10,
                                  )
                    ), #side panel
                    # Main panel for displaying outputs ----
                    mainPanel(
                      # Output: Histogram ----
                      plotOutput(outputId = "distPlot")
                    ) #main panel
                  )),
          tabPanel("Nekaj drugega"), # 2. tab 
          tabPanel("Nekaj tretjega") # 3. tab
          ) # NAV
        )) #shiny in fluid         
                  
                  
                
     
