library(dplyr)
library(readr)
library(tidyr)
library(rvest)
library(tibble)
library(stringr)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

letnice <- c(2010: 2019)

# Prenocitve po vrstah obcin

prenocitve.vrste.obcin <- function(){
  uvoz <- read_csv2("podatki/Prenocitve_vrste_obcin.csv", locale=locale(encoding="Windows-1250"),
                    col_names=c("Tip", letnice)) 
  uvoz <- uvoz[-c(1,2),]
  
  return(uvoz)
}

prenocitve <- prenocitve.vrste.obcin()

tip1 <- prenocitve[,1]



# Sestava tujih turistov po drzavah

tuji.turisti <- function(){
  uvoz <- read_csv2("podatki/Tuji_turisti.csv", locale=locale(encoding="Windows-1250"),
                    col_names=c("Države", letnice)) 
  uvoz <- uvoz[-c(1,2),]

  return(uvoz)
}

tuji.turisti <- tuji.turisti()



# Stevilo vseh gostov

url <- "https://sl.wikipedia.org/wiki/Turizem_v_Sloveniji"

vsi.gosti <- function(){
  gosti <- read_html(url) %>% 
    html_nodes(xpath="//table[@class='wikitable plainrowheaders']") %>%
    .[[1]] %>% html_table(fill=TRUE)
  gosti <- gosti[-c(2, 5)] 
  gosti <- gosti[-c(5), ]
  gosti[, 2] <- parse_number(gosti[, 2])
  gosti[, 3] <- parse_number(gosti[, 3])
  gosti[, 4] <- parse_number(gosti[, 4])
  
  return(gosti)
}

vsi.gosti <-vsi.gosti()



# Stevilo vseh prenocitev

vse.prenocitve <- function(){
  prenocitve <- read_html(url) %>% 
    html_nodes(xpath="//table[@class='wikitable plainrowheaders']") %>%
    .[[5]] %>% html_table(fill=TRUE)
  prenocitve <- prenocitve[-c(2, 5)] 
  prenocitve <- prenocitve[-c(5), ]
  prenocitve[, 2] <- parse_number(prenocitve[, 2])
  prenocitve[, 3] <- parse_number(prenocitve[, 3])
  prenocitve[, 4] <- parse_number(prenocitve[, 4])
  
  return(prenocitve)
}  

vse.prenocitve <- vse.prenocitve()



# Kapacitete vrste obcin

kapacitete.vrste.obcin <- function(){
  uvoz <- read_csv2("podatki/kapacitete_vrste_obcin.csv", locale=locale(encoding="Windows-1250"),
                    col_names=c("Tip", letnice))
  uvoz <- uvoz[-c(1,2),]
  uvoz[, 1] <- tip1 
  
  return(uvoz)
}


kapacitete.vrste.obcin <- kapacitete.vrste.obcin()















