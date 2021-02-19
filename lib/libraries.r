library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)
library(dplyr)
library(readr)
library(tibble)
library(stringr)
library(ggplot2)
library(ggthemes)
library(rgdal)
library(tmap)
library(tmaptools)
library(RColorBrewer)
library(maptools)
library(broom)
library(shinythemes)


options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")

