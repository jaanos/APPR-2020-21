
stolpci1 <- c("", "2002",	"2003",	"2004",	"2005",	"2006",	"2007",	"2008",	"2009",	
              "2010",	"2011",	"2012",	"2013",	"2014",	"2015",	"2016",	"2017",	"2018",	"2019")

stolpciregije <- c("Regija", "2002",	"2003",	"2004",	"2005",	"2006",	"2007",	"2008",	"2009",	
              "2010",	"2011",	"2012",	"2013",	"2014",	"2015",	"2016",	"2017",	"2018",	"2019", "povprecje")

odpadkiletnonaprebivalca <- read_csv2("podatki/2700010S_20210819-091107.csv", 
                     locale=locale(encoding="cp1250"),
                     skip=2)
odpadkiletnonaprebivalca <- odpadkiletnonaprebivalca[, colSums(is.na(odpadkiletnonaprebivalca)) != nrow(odpadkiletnonaprebivalca)]
odpadkiletnonaprebivalca <- odpadkiletnonaprebivalca[-1,]
odpadkiletnonaprebivalca[ , 2:19] <- apply(odpadkiletnonaprebivalca[ , 2:19], 2,           
                                           function(x) as.numeric(as.character(x)))
odpadkiletnonaprebivalca$povprecje <- rowMeans(odpadkiletnonaprebivalca[,2:19])
colnames(odpadkiletnonaprebivalca) <- stolpciregije
odpadkiletnonaprebivalca <- odpadkiletnonaprebivalca %>% 
  pivot_longer(-Regija, names_to="leto", values_to="odpad na prebivalca")

tabela2 <- read_csv2("podatki/2700011S_20210819-090219.csv", 
                     locale=locale(decimal_mark=".", encoding="cp1250"),
                     col_names=stolpci1,
                     skip=3,
                     na="...") %>% rename("regija"=1) %>%
  pivot_longer(-regija, names_to="leto", values_to="vrednost",
               names_transform=list("leto"=parse_number))



tabela6 <- read_csv2("podatki/2777505S_20210819-090428.csv",
                     locale=locale(encoding="cp1250"),
                     skip=2)

tabela6 <- as.data.frame(t(tabela6))
names(tabela6) <- lapply(tabela6[1,], as.character)
tabela6 <- tabela6[-1,1:3]
leta6 <- c("2013","2014","2015","2016", "2017", "2018", "2019")
tabela6 <- tabela6 %>% mutate("LETO"=leta6) 
tabela6 <- tabela6[,c(4,1,2,3)]
stolpci6 <- c("LETO","NACRPANO.SKUPAJ", "PODZEMNE.VODE", "POVRSINSKE.VODE")
colnames(tabela6) <- stolpci6
tabela6$NACRPANO.SKUPAJ <- as.numeric(as.character(tabela6$NACRPANO.SKUPAJ)) / 100000
tabela6$`PODZEMNE.VODE` <- as.numeric(as.character(tabela6$`PODZEMNE.VODE`)) / 10^5
tabela6$`POVRSINSKE.VODE` <- as.numeric(as.character(tabela6$`POVRSINSKE.VODE`)) / 10^5


zadvojnigraf <- tabela6[,-2] %>% pivot_longer(c(-"LETO"), 
                                              names_to="VRSTA.VODE", 
                                              values_to="NACRPANO") 

tabela3 <- read_csv2("podatki/2700020S_20210819-090742.csv", 
                     locale=locale(encoding="cp1250"),
                     skip=2)

obcina <- tabela3$OBČINE
odpadki19kgnaprebivalca <- tabela3$`2019 Nastali komunalni odpadki (kg/prebivalca)`

tabelazakarto <- data.frame(obcina, odpadki19kgnaprebivalca)
tabelazakarto<- tabelazakarto[-1,]
tabelazakarto<- tabelazakarto%>% mutate(obcina=obcina %>% 
                                                        str_replace("Slov[.]", "Slovenskih"))
