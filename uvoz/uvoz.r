library(dplyr)
library(readr)
library(tidyr)
library(rvest)
library(tibble)
library(stringr)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

letnice <- c(2010: 2019)
tipi <- c("Zdraviliske obcine","Gorske obcine","Obmorske obcine","Ljubljana",
        "Mestne obcine","Ostale obcine")

# Prenocitve po vrstah obcin

prenocitve.vrste.obcin <- function(){
  uvoz <- read_csv2("podatki/Prenocitve_vrste_obcin.csv", locale=locale(encoding="Windows-1250"),
                    col_names=c("Tip", letnice)) 
  uvoz <- uvoz[-c(1,2),]
  uvoz <- uvoz[,-c(1)] 
  uvoz <- t(uvoz)
  Stevilo <- c(uvoz[,1], uvoz[,2], uvoz[,3], uvoz[,4], uvoz[,5], uvoz[,6])
  Leta <- rep(2010:2019, 6)
  Tipi <- rep(tipi, 10)
  prenocitve <- data.frame(Tipi, Leta, Stevilo)
  prenocitve[1:10, 1] <- tipi[1]
  prenocitve[11:20, 1] <- tipi[2]
  prenocitve[21:30, 1] <- tipi[3]
  prenocitve[31:40, 1] <- tipi[4]
  prenocitve[41:50, 1] <- tipi[5]
  prenocitve[51:60, 1] <- tipi[6]
  return(prenocitve)
}

prenocitve <- prenocitve.vrste.obcin()




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
  uvoz <- uvoz[,-c(1)] 
  uvoz <- t(uvoz)
  Stevilo <- c(uvoz[,1], uvoz[,2], uvoz[,3], uvoz[,4], uvoz[,5], uvoz[,6])
  Leta <- rep(2010:2019, 6)
  Tipi <- rep(tipi, 10)
  kapacitete <- data.frame(Tipi, Leta, Stevilo)
  kapacitete[1:10, 1] <- tipi[1]
  kapacitete[11:20, 1] <- tipi[2]
  kapacitete[21:30, 1] <- tipi[3]
  kapacitete[31:40, 1] <- tipi[4]
  kapacitete[41:50, 1] <- tipi[5]
  kapacitete[51:60, 1] <- tipi[6]
  return(kapacitete)
}


kapacitete.vrste.obcin <- kapacitete.vrste.obcin()















