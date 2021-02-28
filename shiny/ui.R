shinyUI(
  fluidPage(
    theme = shinytheme("darkly"),
    tabsetPanel(
      tabPanel("AAPLE",
                        titlePanel(title=h1("Napoved dobička na delnico ter cene za podjetje Apple",
                                            align="center",style="margin:80px")),
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput(inputId = "leto",
                                        label = "Leto za katerega želite napoved",
                                        min = 2021,
                                        max = 2025,
                                        value = 2021, 
                                        step = 1, 
                                        sep = ""),
                            radioButtons(inputId = "oblika_e",label="Izbira metode za napoved dobička na delnico",
                                       c("Regresija"="reg","Vgrajena funkcija loess"="loe")),
                            radioButtons(inputId = "check",label="Izbira obdelave podatkov",
                                       c("Osnovni podatki"="osn","Analizirani podatki"="ana"))),
                          mainPanel(
                            fluidRow(plotOutput(outputId = "prvigraf",width="80%",height="400px"),
                                     align = "center",style="border-top:40px"),
                            fluidRow(plotOutput(outputId = "drugigraf",width="80%",height="400px"),
                                     align = "center",style="padding:40px 0px 40px 0px")),
                           )
      )
    )
  )
)   
                       
               
              
                       