# 2. faza: Uvoz podatkov

library(XML)
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(countrycode)


################################################################################
#### 1. del: tabele s podatki o prizorišču - nastopajočih - pesem - točke - mesto
################################################################################

# Zanimajo nas tekmovanja, kjer je nastopila Jugoslavija in Slovenija:
nastopi_jugoslavija <- read_html("https://eurovision.tv/country/yugoslavia")
nastopi_slovenija <- read_html("https://eurovision.tv/country/slovenia")

# tabela podatki_jugoslavija (prizorišče - nastopajoči - pesem - točke - mesto)
podatki_jugoslavija <- nastopi_jugoslavija %>%
  html_nodes(xpath = "//table") %>%
  html_table(fill = TRUE) %>%
  as.data.frame() %>%
  separate(X4, c("TOCKE", "UVRSTITEV"), sep = " points") %>%
  separate(X1, c("KRAJ", "LETO"), sep = " ")
podatki_jugoslavija %>% View
colnames(podatki_jugoslavija) <- c("KRAJ", "LETO", "NASTOPAJOCI", "NASLOV_PESMI", "TOCKE", "UVRSTITEV")

# enkrat je uvrstitev pri točkah:
podatki_jugoslavija$UVRSTITEV[4] <- podatki_jugoslavija$TOCKE[4]
podatki_jugoslavija$TOCKE[4] <- ""

# popravki
podatki_jugoslavija[podatki_jugoslavija == "Eva Sren"] <- "Eva Srsen"
podatki_jugoslavija$UVRSTITEV <- gsub("\\D", "", podatki_jugoslavija$UVRSTITEV)


# tabela podaki_slovenija (prizorišče - nastopajoči - pesem - točke - mesto)
podatki_slovenija <- nastopi_slovenija %>%
  html_nodes(xpath = "//table") %>%
  html_table(fill = TRUE) %>%
  as.data.frame() %>%
  separate(X4, c("TOCKE", "UVRSTITEV"), sep = " points") %>%
  separate(X1, c("KRAJ", "LETO"), sep = " ")

podatki_slovenija %>% View
colnames(podatki_slovenija) <- c("KRAJ", "LETO", "NASTOPAJOCI", "NASLOV_PESMI", "TOCKE", "UVRSTITEV")
podatki_slovenija$UVRSTITEV <- gsub("\\D", "", podatki_slovenija$UVRSTITEV)


# kjer je vrstica Semi-Final, First Semi-Final, Second Semi-Final al pa Grand Final,
# se rezultat iz 4. stolpca vpiše v vrstico, kjer ni semi/final
# vse, ki so semifinal... spremenimo prve tri vrednosti enake kot zgoraj
# najprej prenesemo rezultat iz semi-final gor
v1 <- which(podatki_slovenija$NASTOPAJOCI == "Semi-Final", arr.ind=TRUE)
v2 <- which(podatki_slovenija$NASTOPAJOCI == "First Semi-Final", arr.ind=TRUE)
v3 <- which(podatki_slovenija$NASTOPAJOCI == "Second Semi-Final", arr.ind=TRUE)
v <- sort(c(v1,v2,v3))

for (i in 1:length(v)) {
  podatki_slovenija$TOCKE[v[i]-1] <- podatki_slovenija$TOCKE[v[i]]
  podatki_slovenija$UVRSTITEV[v[i]-1] <- podatki_slovenija$UVRSTITEV[v[i]]
}

podatki_slovenija <- podatki_slovenija[-v,]

u <- which(podatki_slovenija$NASTOPAJOCI == "Grand Final", arr.ind=TRUE)

for (i in 1:length(u)) {
  podatki_slovenija$TOCKE[u[i]-1] <- podatki_slovenija$TOCKE[u[i]]
  podatki_slovenija$UVRSTITEV[u[i]-1] <- podatki_slovenija$UVRSTITEV[u[i]]
} 

podatki_slovenija <- podatki_slovenija[-c(u,32,33),] # zraven stran še tekmovanji rotterdam
rownames(podatki_slovenija) <- NULL


# popravki
podatki_slovenija[podatki_slovenija == "Anej Dean"] <- "Anzej Dezan"
podatki_slovenija[podatki_slovenija == "Naj Bogovi Sliijo"] <- "Naj bogovi slisijo"
podatki_slovenija$NASLOV_PESMI <- gsub("YouTube", "", podatki_slovenija$NASLOV_PESMI)
podatki_slovenija$KRAJ <- gsub("Tel", "Tel Aviv", podatki_slovenija$KRAJ)
podatki_slovenija$LETO <- gsub("Aviv", 2019, podatki_slovenija$LETO)




################################################################################
#### 2. del: podatki o tem, kako sta Slovenija in Jugoslavija glasovali
################################################################################

# oblike url-jev: https://eurovision.tv/mesto-letnica/final/results/yugoslavia
# iz tu vzamemo z regularnimi: 
# https://eurovision.tv/events

vsi_naslovi <- paste(readLines("https://eurovision.tv/events"))
# str_extract(rawHTML, "")
# v značkah so skriti naslovi vseh eventov od lugano-1956 do rotterdam-2021, jaz bom potrebovala od cannes-1961 do tel-aviv-2019
b <- grep("eurovision.tv/event/", vsi_naslovi, value=TRUE)
c <- gsub("<a href=| class=\"absolute top-0 left-0 w-full h-full outline-none\"></a>", "", b)
d <- gsub('\"', "", c)
e <- d[c(3:61)]


# do vključno 2003 /final, od leta 2004 naprej /grand-final
dodatek_povezave_yu <- "/final/results/yugoslavia"
dodatek_povezave_si_1 <- "/final/results/slovenia"
dodatek_povezave_si_2 <- "/grand-final/results/slovenia"


povezave_yu <- rev(paste(e[c(28:34, 36:39,44:59)], dodatek_povezave_yu, sep = ''))
# jugoslavija ni sodelovala na 1977-1980
povezave_si_1 <- rev(paste(e[c(17:19,21:25,27)], dodatek_povezave_si_1, sep = ''))
# slovenija ni nastopala 1994 in 2000
povezave_si_2 <- rev(paste(e[1:16], dodatek_povezave_si_2, sep = ''))


# for (i in c(1:28)) {
#   rezultat <- read_html(povezave_yu[i]) %>%
#     html_nodes(xpath = "//table") %>%
#     .[2] %>%
#     html_table(fill = TRUE)}



# podatki, kako je glasovala Jugoslavija
jugoslavija <- lapply(povezave_yu, function(x) {read_html(x) %>%
    html_nodes(xpath = "//table") %>%
    .[[2]] %>%
    html_table(fill = TRUE) %>%
    rename("Points given" = "Points given", "Country" = "Points given") %>%
    rename("Country" = "Points given", "Points given" = "Country")})


# podatki, kako je glasovala Slovenija do leta 2002
slovenija_1 <- lapply(povezave_si_1, function(x) {read_html(x) %>%
    html_nodes(xpath = "//table") %>%
    .[[2]] %>%
    html_table(fill = TRUE) %>%
    rename("Points given" = "Points given", "Country" = "Points given") %>%
    rename("Country" = "Points given", "Points given" = "Country")})


# slovenija_1[1] so glasovi, ki smo jih dali v millstreetu leta 1993
# države, ki jim daš točke slovenija_1[[1]][2]


## za vsak slučaj obdržim ## slovenija_2 <- lapply(povezave_si_2, function(x) {read_html(x) %>%
## za vsak slučaj obdržim ##     html_nodes(xpath = "//table") %>%
## za vsak slučaj obdržim ##     .[[1]] %>%
## za vsak slučaj obdržim ##     html_table(fill = TRUE)})


# vsi podatki za given and received, jury in televoters SLO 2003-2019
slovenija_2_celo_tockovanje <- lapply(povezave_si_2, function(x) {read_html(x) %>%
    html_nodes(xpath = "//table[@class='w-full']") %>%
    html_table(fill = TRUE)}) %>% print()

# slovenija_2 je celo_tockovanje
# tail(slovenija_2_celo_tockovanje[[16]], 2) ... tabela points given by televoters in given by the jury


# # # poskus_TV <- as.data.frame(tail(slovenija_2_celo_tockovanje[[16]], 2)[1]) %>%
# # #   rename("Points given" ="Points.given.by.televoters", "Country" = "Points.given.by.televoters.1")
# # # 
# # # poskus_jury <- as.data.frame(tail(slovenija_2_celo_tockovanje[[16]], 2)[2]) %>%
# # #   rename("Points given" ="Points.given.by.the.jury", "Country" = "Points.given.by.the.jury.1")
# # # 
# # # poskus_16 <- rbind(poskus_jury,poskus_TV)
# # # 
# # # # katere države se pojavijo 2x in na katerem mestu so po abecedi
# # # # which(summary(as.factor((poskus_16$Country)))>1)
# # # 
# # # # vsota točk za vsako državo
# # # sum(poskus_16$`Points given`[poskus_16$Country == sort(unique(poskus_16$Country))[4]])
# # # 
# # # y <- sort(unique(poskus_16$Country))
# # # x <- rep(0,length(y))
# # # 
# # # for (i in 1:length(y)) {
# # #   x[i] <- sum(poskus_16$`Points given`[poskus_16$Country == sort(unique(poskus_16$Country))[i]])
# # # }
# # # 
# # # poskus_16 <- data.frame(x,y) %>% rename("Points given" = x, "Country" = y)


# tukaj so tabele z GIVEN točkami od 13 do 16, ki jih rabim
slovenija_zadnje4 <- lapply(slovenija_2_celo_tockovanje[c(13:16)], function(x) {tail(x,2)})
slovenija_vmes12 <- lapply(slovenija_2_celo_tockovanje[c(1:12)], function(x) {tail(x,1)})

# za vse od 13:16 seštejemo tv glasove in jury glasove
# točke od televoterjev
televoters <- lapply(slovenija_zadnje4, function(x) {x[1]}) %>%
  lapply(function(x) {data.frame(x)}) %>%
  lapply(function(x) {rename(x, "Points given" ="Points.given.by.televoters", "Country" = "Points.given.by.televoters.1")})


# točke od juryja
jury <- lapply(slovenija_zadnje4, function(x) {tail(x,1)}) %>%
  lapply(function(x) {data.frame(x)}) %>%
  lapply(function(x) {rename(x, "Points given" ="Points.given.by.the.jury", "Country" = "Points.given.by.the.jury.1")})


slovenija_zadnje4_2 <- vector(mode = "list", length = 4)

for (i in 1:4){
  slovenija_zadnje4_2[[i]] <- rbind(jury[[i]], televoters[[i]])
}

# katere države se pojavijo 2x in na katerem mestu so po abecedi
# which(summary(as.factor((poskus_16$Country)))>1)

# vsota točk za vsako državo
sum(poskus_16$`Points given`[poskus_16$Country == sort(unique(poskus_16$Country))[4]])

# to so samo točke po vrtsi od vseh držav zadnja 4 leta
for (j in 1:4){
  for (i in 1:length(unique(slovenija_zadnje4_2[[j]]$Country))){
    print(sum(slovenija_zadnje4_2[[j]]$`Points given`[slovenija_zadnje4_2[[j]]$Country == sort(unique(slovenija_zadnje4_2[[j]]$Country))[i]]))
  }
}



z <- vector(mode = "list", length = 4)
w <- vector(mode = "list", length = 4)
for (j in 1:4) {
  z[[j]] <- sort(unique(slovenija_zadnje4_2[[j]]$Country))
  for (i in 1:length(z[[j]])){
    w[[j]][i] <- sum(slovenija_zadnje4_2[[j]]$`Points given`[slovenija_zadnje4_2[[j]]$Country == z[[j]][i]])
  }
}

for (i in 1:4){
  slovenija_zadnje4_2[[i]] <- data.frame(w[[i]], z[[i]]) %>% rename("Points given" = w..i.., "Country" = z..i..)
}


slovenija_2 <- slovenija_vmes12 %>%
  lapply(function(x) {data.frame(x)}) %>%
  lapply(function(x) {rename(x, "Points given" = Points.given, "Country" = Points.given.1)}) %>%
  append(slovenija_zadnje4_2)



################################################################################
#### 3. del: tabela o glasovanju (prizorišče x država)
################################################################################

# vektor vseh prireditev


kraj <- c(podatki_jugoslavija$KRAJ, podatki_slovenija$KRAJ)
leto <- c(podatki_jugoslavija$LETO, podatki_slovenija$LETO)
prireditve <- paste(kraj, leto, sep = " ")

# dodamo države, ki so kadarkoli sodelovale
link_drzave <- read_html("https://eurovision.tv/countries")

prevod <- function(x) {
  countrycode(x, origin = 'country.name', destination = 'cldr.name.sl',
              custom_match = c("Serbia & Montenegro" = "Srbija in Črna Gora", "Yugoslavia" = "Jugoslavija"))
}

drzave <- link_drzave %>%
  html_nodes(xpath = "//div[@class='flex flex-wrap']//h4[@class='font-bold text-xl leading-tight group-hover:text-blue-600']") %>%
  html_text()
drzave %>% View
vektor_drzave <- gsub("\n", "", drzave)
slo_drzave <- prevod(vektor_drzave)

# združimo države in prireditve v data frame

tabela <- data.frame(matrix(,length(prireditve),length(slo_drzave)))
names(tabela) <- slo_drzave
rownames(tabela) <- prireditve

tabela[is.na(tabela)] <- 0


# vstavljamo vse rezultate v tabelo
# jugoslavija[[1]][[1]][1] == 5: koliko točk je podelila na 1. prireditvi 1. državi
# jugoslavija[[1]][[2]][1] == Luxemburg: kateri državi je na 1. prireditvi podelila največ točk
# length(jugoslavija[[1]][[1]] == 5: število držav, ki je dobilo točke na 1. prireditvi

# tabela[prireditve[3],] : vrstica za 3. prizorišče

# tabela[prireditve[j], jugoslavija[[j]][[2]][k]] <- jugoslavija[[j]][[1]][k]
# : j-to leto, k-ta država za k = 1,...,length(jugoslavija[[j]][1])

# tabela[prireditve[4], jugoslavija[[4]][[2]][3]] <- jugoslavija[[4]][[1]][3]
# dodeli v Copenhagnu 1964 Franciji 1 točko


# izpolnjejujemo za Jugoslavijo
for (j in 1:27) {
  for (k in 1:length(jugoslavija[[j]][[1]])) {
    tabela[prireditve[j], prevod(jugoslavija[[j]][[2]][k])] <- jugoslavija[[j]][[1]][k]
  }
}

# izpolnjujemo za slovenijo_1
for (j in 1:9) {
  for (k in 1:length(slovenija_1[[j]][[1]])) {
    tabela[prireditve[j+27], prevod(slovenija_1[[j]][[2]][k])] <- slovenija_1[[j]][[1]][k]
  }
}

# izpolnjujemo za slovenijo_2
for (j in 1:16) {
  for (k in 1:length(slovenija_2[[j]][[1]])) {
    tabela[prireditve[j+27+9], prevod(slovenija_2[[j]][[2]][k])] <- slovenija_2[[j]][[1]][k]
  }
}

View(tabela)

# vsote danih točk vsaki državi
Vsota <- rep(0, nrow(tabela))
for (i in 1:nrow(tabela)) {
  Vsota[i] <- sum(tabela[i])
}

# v bistvu je smiselno to gledat za jugoslavijo in slovenijo posebej
tabela_jugoslavija <- tabela[c(1:27),]
tabela_slovenija <- tabela[c(28:52),]


Vsota_ju <- rep(0, ncol(tabela_jugoslavija))
for (i in 1:ncol(tabela_jugoslavija)) {
  Vsota_ju[i] <- sum(tabela_jugoslavija[i])
}

tabela_jugoslavija <- rbind(tabela_jugoslavija, Vsota_ju)

Vsota_si <- rep(0, ncol(tabela_slovenija))
for (i in 1:ncol(tabela_slovenija)) {
  Vsota_si[i] <- sum(tabela_slovenija[i])
}

tabela_slovenija <- rbind(tabela_slovenija, Vsota_si)

# treba bo odstraniti države, ki niso nikoli nastopile v letih jugoslavije/slovenije




################################################################################
#### 4. del: tabela za vse države - prvi nastop in število nastopov
################################################################################


povezave_drzave <- paste("https://eurovision.tv/country/", tolower(gsub("\\W+", "-", vektor_drzave)), sep = '')

nastopi_drzave <-lapply(povezave_drzave, function(x) {read_html(x) %>%
    html_nodes(xpath = "//div[@class='space-y-4']//dd[@class='text-sm font-bold']") %>%
    html_text () %>%
    .[[3]] %>%
    lapply(function(x) {gsub("\n", "", x)})
}) %>% unlist() %>% as.numeric()

# to je število nastopov vseh držav

# prvi nastop vseh držav
prvic_drzave <-lapply(povezave_drzave, function(x) {read_html(x) %>%
    html_nodes(xpath = "//div[@class='space-y-4']//dd[@class='text-sm font-bold']") %>%
    html_text () %>%
    .[[4]] %>%
    lapply(function(x) {regmatches(x, regexpr("\\d{4}", x))})
}) %>% unlist() %>% as.numeric()

tabela_nastopi <- data.frame("Drzava" = slo_drzave, "Stevilo_nastopov" = nastopi_drzave, "Prvi_nastop" = prvic_drzave)


################################################################################
# sedaj je treba iz tabela_jugoslavija in slovenija odstraniti države,
# ki niso sodelovale na nobenem tekmovanju (npr jugoslavija pri sloveniji)

# za jugoslavijo: če je prvi nastop po letu 1992, gre država ven, maroko je nastopil samo 1980, takrat jugoslavija NI
# za slovenijo: andora, belorusija, monako (ven), maroko (ven), san marino, slovaška, slovenija (ven), jugoslavija (ven)

tabela_nastopi$Drzava[as.integer(tabela_nastopi$'Prvi_nastop') > 1992]

for (i in 1:length(tabela_jugoslavija)) {
  if (names(tabela_jugoslavija[i]) %in% tabela_nastopi$vektor_drzave[as.integer(tabela_nastopi$prvic_drzave) > 1992]) {
    tabela_jugoslavija[,-i]
  }
}

#tabela_jugoslavija <- tabela_jugoslavija %>% pivot_longer(1:52, names_to = "Drzava", values_to = "Tocke")



d <- tabela_jugoslavija
names <- rownames(d)
rownames(d) <- NULL
data <- cbind(names,d)


data <- data %>% pivot_longer(2:53, names_to = "Drzava", values_to = "Tocke")
# Tabele:
# 1
tabela1 <- podatki_jugoslavija

# 2
tabela2 <- podatki_slovenija

# 3
rownames(tabela_jugoslavija) <- NULL
tabela_jugoslavija <- cbind("Kraj" = kraj[1:27], "Leto" = leto[1:27], tabela_jugoslavija[c(1:27),])
tabela_jugoslavija <- tabela_jugoslavija %>%
  pivot_longer(3:54, names_to = "Drzava", values_to = "Tocke")

tabela3 <- tabela_jugoslavija

# 4
rownames(tabela_slovenija) <- NULL
tabela_slovenija <- cbind("Kraj" = kraj[28:52], "Leto" = leto[28:52], tabela_slovenija[c(1:25),])
tabela_slovenija <- tabela_slovenija %>%
  pivot_longer(3:54, names_to = "Drzava", values_to = "Tocke")

tabela4 <- tabela_slovenija

# 5
tabela5 <- tabela_nastopi
tabela_nastopi_1 <- tabela_nastopi %>% pivot_longer(2:3)




















