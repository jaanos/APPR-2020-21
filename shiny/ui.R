
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
                                            choices = c(sort(unique(tabela2$State)))
                                )
                   ),
                        mainPanel(
                                plotOutput(outputId = "distPlot2")
                                )
                   )), # 2. tab 
          tabPanel("Analiza BDP per capita",
          sidebarLayout(
          sidebarPanel(        
                                selectInput(inputId = "drzava2",
                                            label = "Država:",
                                            choices = c(sort(unique(tabela2$State)))
                        )),
                        mainPanel(
                                plotOutput(outputId = "distPlot3"),
                                plotOutput(outputId = "distPlot4")
                        ) 
                   )),# tab panel 3
        tabPanel("Analiza4",
        sidebarLayout(
        sidebarPanel(
                selectInput(inputId = "tabela",
                            label = "Atribut:",
                            choices = c("HM", "AME", "AM", "emp","HME")
                                )
                ),
                 mainPanel(
                         plotOutput(outputId = "distPlot5")
                        )
              ))
          ) # NAV
        )) #shiny in fluid         
                  
                  
        
     
