#UVOZ IN OBDELAVA PODATKOV


#UVOZ IZ SEC

link <- "https://www.sec.gov/Archives/edgar/data/320193/000032019319000119/a10-k20199282019.htm#sDBCC0D7FC5D05F49A572F9AA0627E992"
stran <- html_session(link) %>% read_html()
podatki_prodaja_svet <- stran %>% html_nodes(xpath="//*[text() = 'Americas:']/ancestor::table[1]") %>%
  .[[1]] %>% html_table() %>% 
  transmute("Podatki"=parse_character(X1, na=""),"2019"=X3,"2018"=X7,"2017"=X11) %>%
  drop_na(Podatki) %>% mutate(Regija=ifelse(`2018` == "", str_replace(Podatki, ":", ""), NA)) %>%
  fill(Regija) %>% pivot_longer(c(-Regija, -Podatki), names_to="Leto", values_to="Vrednost") %>%
mutate(Leto=parse_number(Leto), Vrednost=parse_number(Vrednost)) %>% drop_na(Vrednost)


#UVOZ IZ MORNINGSTARJA

podatki <- read_csv("podatki/AAPL.csv",locale = locale(encoding = "UTF-8"),skip = 2) %>%
  slice(1:16) %>% rename("Podatki"=X1)
podatki<-podatki[,-12]
morningstar <- as.data.frame(t(podatki[-1])) %>% select("V1","V5","V6","V7","V10") %>%
  rownames_to_column(var="A") %>% mutate(A=str_sub(A,1,4))
colnames(morningstar) <- c("Leto","Prodaja","Neto_dobicek",
                           "Neto_dobicek_na_delnico",
                           "Dividenda","Knjigovodska_vrednost")
j<- 1:6
morningstar[,j] <- lapply(morningstar[,j],function(x) {as.numeric(gsub(",","", x))})


#UVOZ IZ YAHOO

yahoo<-as.data.frame(getSymbols("AAPL",from = "2011-01-01", to = "2021-01-01",
                                periodicity = "monthly", auto.assign = FALSE)) %>%
  rownames_to_column(var="Leta")%>%filter(grepl("-12-01",Leta))%>%
  rename("Najvisja_cena"="AAPL.High")%>%mutate(Leto=str_sub(Leta,1,4))%>%
  select("Leto","Najvisja_cena")
yahoo[1]<-lapply(yahoo[1],function(x) as.numeric(x))


#UVOZ QUANDL 

podatki_quandl_pe<-Quandl("MULTPL/SP500_PE_RATIO_YEAR",collapse="annual",
                          api_key="mqxb_zyDyu5wbiG5qtTx",
                          start_date="2010-08-01",
                          end_date="2020-8-31") %>%
  rename("Datum"=Date,"P.E_SP500"=Value) %>% 
  mutate(Leto=as.integer(format(Datum,"%Y"))) %>%
  select("Leto","P.E_SP500" )
podatki_quandl_pe <- podatki_quandl_pe[order(podatki_quandl_pe$Leto),]


podatki_quandl_pb<-Quandl("MULTPL/SP500_PBV_RATIO_YEAR",collapse="annual",
                          api_key="mqxb_zyDyu5wbiG5qtTx",
                          start_date="2011-08-01",
                          end_date="2020-8-31") %>%
  rename("Datum"=Date,"P.B_SP500"=Value) %>%
  mutate(Leto=as.integer(format(Datum,"%Y"))) %>%
  select("Leto","P.B_SP500" )
podatki_quandl_pb <- podatki_quandl_pb[order(podatki_quandl_pb$Leto),]


podatki_quandl_prodaja<-Quandl("MULTPL/SP500_SALES_YEAR",collapse="annual",
                               api_key="mqxb_zyDyu5wbiG5qtTx",
                               start_date="2011-08-01",
                               end_date="2020-8-31") %>%
  rename("Datum"=Date,"Prodaja_SP500"=Value) %>% 
  mutate(Leto=as.integer(format(Datum,"%Y"))) %>% 
  select("Leto","Prodaja_SP500" )
podatki_quandl_prodaja <- podatki_quandl_prodaja[order(podatki_quandl_prodaja$Leto),]  


podatki_quandl_earning<-Quandl("MULTPL/SP500_EARNINGS_YEAR",collapse="annual",
                               api_key="mqxb_zyDyu5wbiG5qtTx",
                               start_date="2011-08-01",
                               end_date="2020-8-31") %>%
  rename("Datum"=Date,"Earning_SP500"=Value) %>%
  mutate(Leto=as.integer(format(Datum,"%Y"))) %>%
  select("Leto","Earning_SP500" )
podatki_quandl_earning <- podatki_quandl_earning[order(podatki_quandl_earning$Leto),] 
 

podatki_quandl_bv<-Quandl("MULTPL/SP500_BVPS_YEAR",collapse="annual",
                          api_key="mqxb_zyDyu5wbiG5qtTx",
                          start_date="2011-08-01",
                          end_date="2020-8-31") %>%
  rename("Datum"=Date,"BV_SP500"=Value) %>%
  mutate(Leto=as.integer(format(Datum,"%Y"))) %>%
  select("Leto","BV_SP500" )
podatki_quandl_bv <- podatki_quandl_bv[order(podatki_quandl_bv$Leto),]  


podatki_quandl_dividenda <- Quandl("MULTPL/SP500_DIV_YEAR",collapse="annual",
                                   api_key="mqxb_zyDyu5wbiG5qtTx",
                                   start_date="2011-08-01",
                                   end_date="2020-8-31")%>%
  rename("Datum"=Date,"Dividenda_SP500"=Value) %>% 
  mutate(Leto=as.integer(format(Datum,"%Y"))) %>%
  select("Leto","Dividenda_SP500" )
podatki_quandl_dividenda <- podatki_quandl_dividenda[order(podatki_quandl_dividenda$Leto),]

#KONTINENTI

kontinenti <- read_csv("podatki/kontinenti1.csv",locale = locale(encoding = "UTF-8")) %>%
  select("Continent_Name","Country_Name","Three_Letter_Country_Code") %>%
  rename("GU_A3"="Three_Letter_Country_Code")


#IZRACUNI PE PB

PE <- select(morningstar,"Leto","Neto_dobicek_na_delnico") %>% left_join(yahoo)%>%
  mutate(PE=Najvisja_cena/Neto_dobicek_na_delnico) %>% select("Leto","PE")
  
PB <- select(morningstar,"Leto","Knjigovodska_vrednost") %>% left_join(yahoo)%>%
  mutate(PB=Najvisja_cena/Knjigovodska_vrednost)%>%
  select("Leto","PB")
  

#FUNKCIJA ZA RAST

rast <- function(tabela,b){
  a <- mutate(tabela,Rast=((tabela[[b]]-lag(tabela[[b]]))/lag(tabela[[b]]))*100)%>%
    select("Leto","Rast")
  return(a)
} 


#IZRACUNANE RASTI

Rast_prodaje <- rast(morningstar,"Prodaja")
  
Rast_dobicka <-  rast(morningstar,"Neto_dobicek")
  
Rast_knjigovodske_vrednosti <-  rast(morningstar,"Knjigovodska_vrednost")
  
Rast_dividende <- rast(morningstar,"Dividenda")

Rast_SP_prodaja <- rast(podatki_quandl_prodaja,"Prodaja_SP500")

Rast_SP_dobicka <- rast(podatki_quandl_earning,"Earning_SP500")
  
Rast_SP_knjigovodske_vrednosti <- rast(podatki_quandl_bv,"BV_SP500")

Rast_SP_dividende <- rast(podatki_quandl_dividenda,"Dividenda_SP500")




