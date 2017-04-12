# Analiza podatkov s programom R, 2016/17

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2016/17

## Slovenska podjetja po panogah

Seminarska naloga bo vključevala različne podatke o slovenskih podjetjih na splošno. Najprej njihovo število po posameznih regijah, nato pa podatki iz poslovanja podjetij glede na panogo.
V zadnjem delu, bom v analizo za primerjavo vključila 6 konkretnih večjih slovenskih podjetij (Revoz, Cimos, Krka, Lek, Gen-I, HSE), med katerimi bosta po dva pripadala isti panogi.

1.) PODJETJA PO REGIJAH:

* podatki iz SURS-a: http://pxweb.stat.si/pxweb/Dialog/varval.asp?ma=1418806S&ti=Podjetja+po+kohezijskih+in+statisti%E8nih+regijah%2C+Slovenija%2C+letno&path=../Database/Ekonomsko/14_poslovni_subjekti/01_14188_podjetja/&lang=2

* podatki v tabeli: leto (2008-2015), Slovenske regije, število podjetij, število oseb (,ki delajo na podjetje), prihodek (v 1000 EUR)


2.)	Poslovanje podjetij po panogah, Slovenija, četrtletno:

* podatki iz SURS-a: http://pxweb.stat.si/pxweb/Dialog/varval.asp?ma=1430310S&ti=&path=../Database/Ekonomsko/14_poslovni_subjekti/10_14303_cetrt_poslovanje_podj/&lang=1

* podatki v tabeli: leto (2011-2016), SKD dejavnost, stopnja lastniškosti financiranja, produktivnost kapitala, dobiček na sredstva, kazalnik izvozne usmerjenosti



*	Prihodki podjetij po dejavnostih (samo za 2014, zato vrejetno ne bom vključila): http://pxweb.stat.si/pxweb/dialog/varval.asp?ma=H049S&ti=&path=..%2FDatabase%2FHitre_Repozitorij%2F&xu=&yp=&lang=2

3.) Velika slovenska podjetja:

* CIMOS: http://www.cimos.si/index.php?page=static&item=98 , http://www.cimos.si/index.php?page=letna_porocila&item=99

* REVOZ: http://www.revoz.si/bin?bin.svc=obj&bin.id=DB63EA63-7CFA-79F1-D73B-C4B6B9E960A0

* Gen-I: http://www.gen-i.si/media/1560/gen-i-letno-poroc-ilo-2015_k3.pdf

* HSE: http://www.hse.si/si/files/default/letna-porocila/Letno%20poro%c4%8dilo%20d%c5%bebe%20in%20skupine%20HSE%202015%20SLO.pdf

* Krka:  http://www.krka.si/media/doc/si/za_vlagatelje/2016/KRKA%20LP%20SI%206-6-2016%20LR.pdf

* Lek:

* Petrol: https://www.petrol.si/sites/www.petrol.si/files/attachment/letno_porocilo_petrol_2015.pdf

* OMV SLO:

* podatki iz različnih tabel: imena podjetij, Kraj , SKD dejavnost, stopnja lastniškosti financiranja, produktivnost kapitala, dobiček na sredstva, kazalnik izvozne usmerjenosti

Pod točko 2 bom iz četrtletnih podatkov poizkušala izračunati letne podatke za posamezne poslovne kazalnike. Nato bom naredila primerjavo med konkretnimi podjetji iz panoge in podatki za panogo.



## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`. Ko ga prevedemo,
se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Zemljevidi v obliki SHP, ki jih program pobere, se
shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `maptools` - za uvoz zemljevidov
* `sp` - za delo z zemljevidi
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `reshape2` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
