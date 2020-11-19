library(knitr)
library(rvest)
library(gsubfn)
library(reshape2)
library(shiny)
library(tidyr)
library(dygraphs)
library(xts)
library(tidyverse)
library(lubridate)
library(tmap)
library("readxl")
# library("openxlsx")
source("https://raw.githubusercontent.com/jaanos/APPR-2019-20/master/lib/uvozi.zemljevid.r")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")

options(gsubfn.engine="R")


