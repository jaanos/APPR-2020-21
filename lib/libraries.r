library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(scales)
library(tmap)
library(maps)
library(rworldmap)
library(shiny)
library(shinythemes)




options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")
