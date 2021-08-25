# Analiza podatkov s programom R, 2020/21

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2020/21

* [![Shiny](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/jaanos/APPR-2020-21/master?urlpath=shiny/APPR-2020-21/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/jaanos/APPR-2020-21/master?urlpath=rstudio) RStudio

## Tematika

V svojem projektu bom analizirala meddržavne selitve v Slovenijo ter iz Slovenije. Nekatere pridobljene podatke bom primerjala tudi z evropskimi.

Podatke bom pretežno pridobila iz spodnjih dveh strani:

Statistični urad Republike Slovenije (https://pxweb.stat.si/SiStat/sl/Podrocja/Index/100/prebivalstvo) in
EUROSTAT (https://ec.europa.eu/eurostat/home)

Analizirala bom:

1) Meddržavne selitve (priseljevanja in odseljevanja) po državi državljanstva in spolu, letno
(https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/05N1008S.px)

Moj cilj je ugotoviti, kako so se skozi leta v Sloveniji odseljevali in priseljevali ljudje, iz kje so se preselili, ter kam so se preselili. Za ta namen bom potrebovala tudi naslednjo tabelo:
2) Meddržavne selitve po državljanstvu, državi prejšnjega/prihodnjega prebivališča,spolu  Slovenija, letno
(https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/05N1010S.px)

Rada bi ugotovila trende priseljevanja, kaj so razlogi za tem, ter tudi kakšni ljudje se k nam priseljujejo ter kakšni k nam odseljujejo. Rada bi raziskala njihovo starost, izobrazbo ter kaj priseljenci prinašajo na naš trg dela, ter kaj izgubljamo od ljudi, ki so se odselili iz Slovenije. Za to analizo bom potrebovala spodnje tabele:

3) Meddržavne selitve po starostnih skupinah, državljanstvu in spolu, letno
(https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/05N1006S.px)

4) Meddržavni selivci, stari 15 ali več let po izobrazbi, Slovenija, letno
(https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/H146S.px)

Za priseljene v Slovenijo bi rada analizirala, kaj je v večini njihov namen priseljevanja v Slovenijo, ter v kakših dejavnostih so zaposleni. Prav tako me bo za odseljene zanimalo, s čim se ukvarjajo oni.

5) Priseljeni tujci po namenu priselitve in državi državljanstva, Slovenija, letno
(https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/05N3102S.px)
6) Priseljeni zaposleni prebivalci po dejavnosti, državljanstvu in spolu, Slovenija, letno
(https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/05N3117S.px)

7) Odseljeni zaposleni prebivalci po dejavnosti, državljanstvu in spolu, Slovenija, letno
(https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/05N3220S.px)

Obenem bom za glavne države priseljevanja ter odseljevanja pogledala njihov BDP v zadnjih letih in poskušala ugotoviti, ali ekonomske razmere v državi vplivajo na odseljevanje ljudi ter priseljevanje. Za ta namen bom podatke črpala iz Wikipedije.

8) Spreminjanje GDP držav v Evropi  (https://en.wikipedia.org/wiki/List_of_sovereign_states_in_Europe_by_GDP_(nominal))

Za konec pa bom pogledala še agregatno priseljevanje, odseljevanje v druge evropske države in poskušala to primerjati s Slovenijo. Za ta namen bom podatke črpala iz Eurostat-a.

9) Priseljevanje po državah, spolu, letno 
(https://appsso.eurostat.ec.europa.eu/nui/submitViewTableAction.do)

10) Odseljevanje po državah, spolu, letno
(https://appsso.eurostat.ec.europa.eu/nui/submitViewTableAction.do)

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
