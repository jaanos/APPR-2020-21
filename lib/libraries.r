library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)

options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")

# Uvoz podatkov
source("uvoz/uvoz1.r", encoding = "UTF-8")

# Vizualizacije
source("vizualizacija/vizualizacija1.r", encoding = "UTF-8")
