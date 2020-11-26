library(readxl)
library(openxlsx)

podatki1 <- read_xlsx('projekt/ZDA.xlsx')
podatki2 <- read_xlsx('projekt/SLO.xlsx')

library(dplyr)

View(podatki1)