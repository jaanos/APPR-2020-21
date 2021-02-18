
sl <- locale("sl", decimal_mark=",", grouping_mark=".")

letnice <- c(2010: 2019)
tipi <- c("Zdraviliške občine","Gorske občine","Obmorske občine","Ljubljana",
        "Mestne občine","Ostale občine")

# Prenocitve po vrstah obcin

prenocitve.vrste.obcin <- function(){
  uvoz <- read_csv2("podatki/Prenocitve_vrste_obcin.csv", 
                    locale=locale(encoding="Windows-1250"),
                    col_names=c("Tip", letnice))
  uvoz <- uvoz[-c(1,2),]
  uvoz[, 1] <- tipi
  uvoz <- pivot_longer(uvoz, -Tip, names_to = "Leto", values_to = "Stevilo") %>%
    arrange(Leto)
  uvoz$Leto <- as.numeric(uvoz$Leto)
  uvoz$Stevilo <- as.numeric(uvoz$Stevilo)
  uvoz %>% data.frame()

  return(uvoz)
}

prenocitve.tipi <- prenocitve.vrste.obcin()



# Prihodi po obcinah

obcine <- function(datoteka, leto=2019){
  uvoz <- read_csv2(datoteka, locale=locale(encoding="Windows-1250"),
                              col_names=c("Obcina", "Leto", "Stevilo"))
  uvoz <- uvoz[-c(1,2),]
  uvoz$Leto <- paste(leto)
  uvoz$Stevilo <- parse_number(uvoz$Stevilo) 
  uvoz[is.na(uvoz)] <- 0
  uvoz %>% data.frame()
  uvoz$Obcina[100] <- "Mežica"

  
  return(uvoz)
}

obcine_prihodi_2019 <- obcine("podatki/prihodi_po_obcinah_2019.csv")
obcine_prihodi_2018 <- obcine("podatki/prihodi_po_obcinah_2018.csv", 2018)
obcine_prihodi <- rbind(obcine_prihodi_2018, obcine_prihodi_2019)



# Stevilo vseh gostov

url <- "https://sl.wikipedia.org/wiki/Turizem_v_Sloveniji"

vsi.gosti <- function(){
  gosti <- read_html(url) %>% 
    html_nodes(xpath="//table[@class='wikitable plainrowheaders']") %>%
    .[[1]] %>% html_table(fill=TRUE)
  gosti <- gosti[-c(2, 5)] 
  gosti <- gosti[-c(5), ]
  colnames(gosti) <- c("Leto", "Tuji", "Domači", "Skupaj")
  gosti <- pivot_longer(gosti, -Leto,
                             names_to = "Tip", values_to = "Stevilo")
  gosti <- gosti[, c("Tip", "Leto", "Stevilo")]
  gosti$Stevilo <- gsub("\\.","", gosti$Stevilo)
  gosti$Stevilo <- as.numeric(gosti$Stevilo)
  gosti$Leto <- as.numeric(gosti$Leto)
  gosti %>% data.frame()
  gosti <- arrange(gosti, Tip)
  
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
  colnames(prenocitve) <- c("Leto", "Tuji", "Domaci", "Skupaj")
  prenocitve <- pivot_longer(prenocitve, -Leto,
                             names_to = "Tip", values_to = "Stevilo")
  prenocitve <- prenocitve[, c("Tip", "Leto", "Stevilo")]
  prenocitve$Stevilo <- gsub("\\.","", prenocitve$Stevilo)
  prenocitve$Stevilo <- as.numeric(prenocitve$Stevilo)
  prenocitve$Leto <- as.numeric(prenocitve$Leto)
  prenocitve <- arrange(prenocitve, Tip)
  
  return(prenocitve)
}  

vse.prenocitve <- vse.prenocitve()



# Kapacitete vrste obcin

kapacitete.vrste.obcin <- function(){
  uvoz <- read_csv2("podatki/kapacitete_vrste_obcin.csv", 
                    locale=locale(encoding="Windows-1250"),
                    col_names=c("Tip", letnice))
  uvoz <- uvoz[-c(1,2),]
  uvoz[, 1] <- tipi
  uvoz <- pivot_longer(uvoz, -Tip, names_to = "Leto", values_to = "Stevilo")
  uvoz$Leto <- as.numeric(uvoz$Leto)
  uvoz$Stevilo <- as.numeric(uvoz$Stevilo)
  uvoz %>% data.frame()
  
  return(uvoz)
}

kapacitete.vrste.obcin <- kapacitete.vrste.obcin()

















