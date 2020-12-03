# Analiza podatkov s programom R, 2020/21
# Analiza sekundarnega in terciarnega izobraževanja
## Ana Marija Okorn

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2020/21

* [![Shiny](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/OkornA18/APPR-2020-21/master?urlpath=shiny/APPR-2020-21/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/OkornA18/APPR-2020-21/master?urlpath=rstudio) RStudio

## Tematika

V svojem projektu bom analizirala vključenost dijakov v srednje šole ter diplomantov v ustanove namenjene terciarnemu izobraževanju. Vse podatke, ki jih bom vključila v svojo projektno nalogo, sem dobila na [Statističnem uradu republike Slovenije](https://www.stat.si/StatWeb/) v obliki html in csv. Analizirala bom podatke od leta 2009-2018.

V tabele bom vključila število dijakov v posameznih statističnih regijah. Prikazala bom tudi število dijakov po spolu po posameznih vrstah izobraževanja. Prikazala bom tudi število diplomantov na leto glede na vrsto izobraževanja in načinu študija in na spol, kasneje jih bom razdelila tudi po statističnih regijah. Eden izmed ciljev projektne naloge je ugotoviti delež dijakov in diplomantov posameznih regij glede na število vseh prebivalcev v regiji. Prav tako pa me zanima kakšen delež diplomantov diplomira na rednem in koliko na izrednem študiju. Rada pa bi analizirala tudi kako se je število tujih študentov v Sloveniji z leti spreminjalo.

Moji podatki bodo v naslednjih tabelah:

 1. tabela Dijaki in diplomanti terciarnega izobraževanja po statistični regiji stalnega prebivališča, Slovenija, 
 
    Stolpci: -Regija, -Leto, -Kategorija (dijaki, diplomanti), -Število

 2. tabela Dijaki po spolu in vrsti izobraževanja, Slovenija, letno 
 
    Stolpci: -Vrsta izobraževanja, -Leto, -Spol(moški, ženski), -Število

 3. tabela Diplomanti terciarnega izobraževanja po vrsti izobraževanja in spolu, Slovenija, letno 
 
    Stolpci: -Vrsta izobraževanja, -Leto, -Spol(moški, ženski), -Število

 4. tabela Diplomanti terciarnega izobraževanja po vrsti izobraževanja in načinu študija, Slovenija, letno 
 
    Stolpci: -Vrsta izobraževanja, -Leto, -Način študija, -Število
  
 5. tabela Število prebivalcev po regijah, letno
 
    Stolpci: -Regija, -Leto, -Število
  

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
