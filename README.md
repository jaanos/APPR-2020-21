# Analiza podatkov s programom R, 2020/21

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2020/21

* [![Shiny](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/UrbanRupnik/APPR-2020-21/master?urlpath=shiny/APPR-2020-21/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/UrbanRupnik/APPR-2020-21/master?urlpath=rstudio) RStudio

## Analiza razpoložljivega dohodka gospodinjstev v Sloveniji

V svoji projektni nalogi bom analiziral razpoložljivi dohodek slovenskih gospodinjstev zadnjih let. Opazoval bom povprečni dohodek na člana gospodinjstva po opravljenih socialnih transferjih glede na **vrsto dohodka**, **spol**, **starost** in **izobrazbo** ter **dohodek po statističnih regijah Slovenije**.

## Podatki

Svoje podatke bom črpal s strani [SiStat](https://pxweb.stat.si/SiStat/sl) v obliki CSV datotek (ločeno s podpičjem, z glavo) ter v HTML obliki. Shranjeni so v istoimenski mapi [podatki](https://github.com/UrbanRupnik/APPR-2020-21/tree/master/podatki).

### Tabele

* [Razpoložljivi dohodek gospodinjstev po starosti in spolu (EUR)](https://pxweb.stat.si:443/SiStatData/sq/1229)
* [Razpoložljivi dohodek gospodinjstev glede na doseženo izobrazbo in spol (EUR)](https://pxweb.stat.si:443/SiStatData/sq/1230)
* [Razpoložljivi dohodek gospodinjstev po statističnih regijah (EUR)](https://pxweb.stat.si:443/SiStatData/sq/1228)
* [Povprečni razpoložljivi dohodek gospodinjstev po vrstah dohodka (EUR)](https://pxweb.stat.si:443/SiStatData/sq/1214)

## Plan dela

### Uvoz

Podatke sem uvozil iz CSV datotek in spleta ter jih prečistil v mapi [uvoz](https://github.com/UrbanRupnik/APPR-2020-21/tree/master/uvoz). Urejene tabele sem nato v CSV obliki izvozil nazaj v mapo [podatki](https://github.com/UrbanRupnik/APPR-2020-21/tree/master/podatki).

### Analiza

Pri analizi bom najprej grafično predstavil dohodek glede na vrsto dohodka in pripravil zemljevid po regijah. Nato pa bom iskal povezave med ostalimi spremenljivkami (npr. ali je razmerje med spoloma podobno razmerju med določeno izobrazbo itd.), glede na obnašanje v zadnjih letih pripravil predikcijo za prihodnost in tudi ostale rezultate vizualiziral.

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

## Avtor
* Urban Rupnik
