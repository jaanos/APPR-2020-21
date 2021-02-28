# Analiza podatkov s programom R, 2020/21

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2020/21

* [![Shiny](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/ian-spiller/APPR-2020-21/master?urlpath=shiny/APPR-2020-21/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/ian-spiller/APPR-2020-21/master?urlpath=rstudio) RStudio

## Analiza podjetja Apple v primerjavi s S&P500

### Osnovna ideja

Za projektno nalogo bom analiziral podatke podjetja Apple in jih primerjal s borznim indeksom S&P 500.
Pogledal bom rast prodaje,neto dobička,knjigovodske vrednosti in dividend od S$P 500 in podjetja Appla v zadnjih 10tih letih,
ter primerjal rasti med seboj.
Nato bom primerjal še koeficenta P/E (cena/dobiček na delnico),P/B (cena/knjigovodska vrednost na delnico) in profitno maržo
od podjetja Appla in S&P500 za zadnjih 10 let. Na zemljevidu sveta bom tudi prikazal prodajo podjetja Apple po svetu.

### Potek dela

* Na podalgi dobljenih podatkov bom naredil analizo parametrov, ki sem jih navedel zgoraj.
* V grafih bom prikazal rezultate analize.
* Nato bom s vgrajeno metodo loess in s metodo linearne regresije poskušal napovedati prihodnje dobičke. 
Poiskal bom povezavo med dobičkom in ceno delnice s pomočjo linearne regresije po metodi najmanjših kvadratov.
Iz napovedanega dobička bom tako poskušal napovedati ceno delnice v prihodnje.
* Nakoncu bom naredil še aplikacijo shiny, kjer bo lahko uporabnik sam določil za katero leto želi 
napovedati dobičke ter po kakšni mtodi.


### Tabele

1. [podatki_prodaja_svet](https://www.sec.gov/Archives/edgar/data/320193/000032019319000119/a10-k20199282019.htm#sDBCC0D7FC5D05F49A572F9AA0627E992)- podatki o prodaji in dobičku podjetja Apple po svetu
* `Podatki` - spremenljivka: vrsta meritve (Prodaja, dobiček pred davkom)
* `Regija` - spremenljivka: kontinet
* `Leto` - spremenljivka: leto meritve (število:2017-2019)
* `Vrednost` - meritev: vrednost prodaje in dobička pred davkom v miljonih USD (število)

2. [morningstar](https://financials.morningstar.com/ratios/r.html?t=0P000000GY&culture=en&platform=sal)
* `Leto` - spremenljivka: leto meritve (število:2011-2020)
* `Prodaja` - meritev: prodaja v milijonih USD (število)
* `Neto_dobicek` - meritev: Dobiček po davku v milijonih USD (število)
* `Neto_dobiček na delnico` - meritev: Dobiček po davku na posamezno delnico v USD (število)
* `Dividenda - meritev`: Dividenda v USD (število)
* `Knjigovodska_vrednost`: Knjigovodska vrednost posamezne delnice v USD (število)

3. [yahoo](https://finance.yahoo.com/quote/AAPL/history/) - za uvoz podatkov sem uporabil knjižnico quantmod 
* `Najvišja_cena` - meritev: najvišja cena Applove delnice 1. decembra vsako leto v USD (število)
* `Leto` - spremenljivka: leto meritve (število:2011-2020)

4. [podatki_quandl_pe](https://www.quandl.com/data/MULTPL/SHILLER_PE_RATIO_MONTH-Shiller-PE-Ratio-by-Month) - za uvoz sem uporabil knjižnico Quandl
* `Leto` - spremenljivka: leto meritve (število:2011-2020)
* `P.E_SP500` - meritev: P/E koeficient indeksa S&P 500 (število)

5. [podatki_quandl_pb](https://www.quandl.com/data/MULTPL/SP500_PBV_RATIO_YEAR-S-P-500-Price-to-Book-Value-by-Year) - za uvoz sem uporabil knjižnico Quandl
* `Leto` - spremenljivka: leto meritve (število:2011-2020)
* `P.B_SP500` - meritev: P/B koeficient indeksa S&P 500 (število)

6. [podatki_quandl_prodaja](https://www.quandl.com/data/MULTPL/SP500_SALES_YEAR-S-P-500-Sales-by-Year) - za uvoz sem uporabil knjižnico Quandl
* `Leto` - spremenljivka: leto meritve (število:2011-2020)
* `Prodaja_SP500` - meritev: Prodaja vseh podjetji v S&P 500 v milijardah USD (število)

7. [podatki_quandl_earning](https://www.quandl.com/data/MULTPL/SP500_EARNINGS_YEAR-S-P-500-Earnings-by-Year) - za uvoz sem uporabil knjižnico Quandl
* `Leto` - spremenljivka: leto meritve (število:2011-2020)
* `Earning_SP500` - meritev: Dobiček po davkih vseh podjetji v S&P 500 v milijardah USD (število)

8. [podatki_quandl_bv](https://www.quandl.com/data/MULTPL/SP500_BVPS_YEAR-S-P-500-Book-Value-Per-Share-by-Year) - za uvoz sem uporabil knjižnico Quandl
* `Leto` - spremenljivka: leto meritve (število:2011-2020)
* `BV_SP500` - meritev: Knjigovodska vrednost "delnice" indeksa S&P 500 v USD (vsota vseh knjigovodskih vrednosti na delnico podjetji v S&P 500) (število) 

9. [podatki_quandl_dividenda](https://www.quandl.com/data/MULTPL/SP500_DIV_YEAR-S-P-500-Dividend-by-Year) - za uvoz sem uporabil knjižnico Quandl
* `Leto` - spremenljivka: leto meritve (število:2011-2020)
* `Dividenda_SP500` - meritev: Vsota vseh dividend v S&P 500 (število)

10. [kontinenti](https://datahub.io/JohnSnowLabs/country-and-continent-codes-list)
* `Continent_Name` - spremenljivka: ime kontinenta (število)
* `Country_Name` - spremenljivka: ime države (število)
* `GU_A3` - spremenljivka: kratica države  (število)


## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `rgdal` - za uvoz zemljevidov
* `rgeos` - za podporo zemljevidom
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `tidyr` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `mosaic` - za pretvorbo zemljevidov v obliko za risanje z `ggplot2`
* `maptools` - za delo z zemljevidi
* `tmap` - za izrisovanje zemljevidov
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)

## Binder

Zgornje [povezave](#analiza-podatkov-s-programom-r-202021)
omogočajo poganjanje projekta na spletu z orodjem [Binder](https://mybinder.org/).
V ta namen je bila pripravljena slika za [Docker](https://www.docker.com/),
ki vsebuje večino paketov, ki jih boste potrebovali za svoj projekt.

Če se izkaže, da katerega od paketov, ki ji potrebujete, ni v sliki,
lahko za sprotno namestitev poskrbite tako,
da jih v datoteki [`install.R`](install.R) namestite z ukazom `install.packages`.
Te datoteke (ali ukaza `install.packages`) **ne vključujte** v svoj program -
gre samo za navodilo za Binder, katere pakete naj namesti pred poganjanjem vašega projekta.

Tako nameščanje paketov se bo izvedlo pred vsakim poganjanjem v Binderju.
Če se izkaže, da je to preveč zamudno,
lahko pripravite [lastno sliko](https://github.com/jaanos/APPR-docker) z želenimi paketi.

Če želite v Binderju delati z git,
v datoteki `gitconfig` nastavite svoje ime in priimek ter e-poštni naslov
(odkomentirajte vzorec in zamenjajte s svojimi podatki) -
ob naslednjem zagonu bo mogoče delati commite.
Te podatke lahko nastavite tudi z `git config --global` v konzoli
(vendar bodo veljale le v trenutni seji).
