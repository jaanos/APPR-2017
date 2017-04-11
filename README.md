# Analiza podatkov s programom R, 2016/17

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2016/17

## Slovenska podjetja po panogah

V seminarski nalogi bom analizirala slovenska podjetja. Analiza bo potekala v dveh delih, prvi del v katerem bom na podlagi podatkov iz SURS-a prikazala število podjetij in dohodkov po regijah, bo splošnejši. Tukaj bom tudi analizirala poslovanje podjetij po panogah, preko pomembnejših kazalnikov (npr. stopnjo lastniškosti financiranja, koeficjent usmerjenosti k izvozu, dobiček na sredstva).

V drugem delu bom dodala analizo nekaj konkretnih večjih slovenskih podjetij (okrog 6: Revoz, Cimos, Krka, Lek, Gen-I, HSE), med katerimi bosta po dve podjetji iz iste oziroma sorodne panoge. 

Podatki iz SURS-a:
•	Poslovanje podjetij po panogah, Slovenija, četrtletno:
http://pxweb.stat.si/pxweb/Dialog/varval.aspma=1430310S&ti=&path=../Database/Ekonomsko/14_poslovni_subjekti/10_14303_cetrt_poslovanje_podj/&lang=1
•	št. podjetij po regijah: 
http://pxweb.stat.si/pxweb/Dialog/varval.aspma=1418806S&ti=Podjetja+po+kohezijskih+in+statisti%E8nih+regijah%2C+Slovenija%2C+letno&path=../Database/Ekonomsko/14_poslovni_subjekti/01_14188_podjetja/&lang=2
•	Prihodki podjetij po dejavnostih(samo za 2014, mogoče ne bom vključila!!!): 
http://pxweb.stat.si/pxweb/dialog/varval.asp?ma=H049S&ti=&path=..%2FDatabase%2FHitre_Repozitorij%2F&xu=&yp=&lang=2

DRUGI PODATKI:
CIMOS: http://www.cimos.si/index.php?page=static&item=98
http://www.cimos.si/index.php?page=letna_porocila&item=99
REVOZ: http://www.revoz.si/bin?bin.svc=obj&bin.id=DB63EA63-7CFA-79F1-D73B-C4B6B9E960A0
Gen-I: http://www.gen-i.si/media/1560/gen-i-letno-poroc-ilo-2015_k3.pdf
HSE: http://www.hse.si/si/files/default/letna-porocila/Letno%20poro%c4%8dilo%20dru%c5%bebe%20in%20skupine%20HSE%202015%20SLO.pdf
Krka:  http://www.krka.si/media/doc/si/za_vlagatelje/2016/KRKA%20LP%20SI%206-6-2016%20LR.pdf
Lek:
Petrol: https://www.petrol.si/sites/www.petrol.si/files/attachment/letno_porocilo_petrol_2015.pdf
OMV SLO:

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
