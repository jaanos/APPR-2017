# 2. faza: Uvoz podatkov:

# Knjižnjice:
Encoding("UTF-8")
library(dplyr)
library(rvest)

library(reader)
library(stats)
library(tidyr)

# Nastavitev relativne poti do csv datotek:

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
setwd('..')
trenutno = setwd(Sys.getenv("HOME"));


# Funkcije, ki uvozijo podatke iz datoteke druzine csv:

uvozi.meritve <- function(trenutno) {
  fpot = file.path(trenutno, 'podatki', "meritve.csv");
  tabela <- read.csv2(fpot,
                      skip = 1,
                      header = FALSE,
                      col.names = c("obcina", "vrsta meritve", "leto", "vrednost"),
                      na=c("", " ")
  )
  tabela <- tabela %>% fill(1:2)
  vrstice.z.na <- apply(tabela, 1, function(x){any(is.na(x))})
  tabela <- tabela[!vrstice.z.na,]
  return(tabela)
}

uvozi.place <- function(trenutno) {
  fpot = file.path(trenutno, 'podatki', "place.csv");
  tabela <- read.csv2(fpot,
                      col.names = c("obcina","leto","povprecna mesecna bruto placa"),
                      skip = 3,
                      header = FALSE,
                      na=c("", " ","-")
  )
  tabela <- tabela %>% fill(1) 
  vrstice.z.na <- apply(tabela, 1, function(x){any(is.na(x))})
  tabela <- tabela[!vrstice.z.na,]
  return(tabela)
}


uvozi.turizem <- function(trenutno) {
  fpot = file.path(trenutno, 'podatki', "turizem.csv");
  tabela <- read.csv2(fpot,
                      skip = 4,
                      na=c("", " ","z","-"),
                      col.names = c("obcina", "leto", "parameter" , "vrednost"),
                      header = FALSE
  )
  tabela <- tabela %>% fill(1:2)
  vrstice.z.na <- apply(tabela, 1, function(x){any(is.na(x))})
  tabela <- tabela[!vrstice.z.na,]
  return(tabela)  
}

# Funkcija, ki uvozi podatke iz Wikipedije:

uvozi.obcine <- function() {
  encoding = "Windows-1250"
  link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[1]] %>% html_table(dec = ",")
  colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
                        "ustanovitev", "pokrajina", "regija", "odcepitev")
  tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
  tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
  tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
  for (col in colnames(tabela)) {
    tabela[tabela[[col]] == "-", col] <- NA
  }
  for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
    if (is.numeric(tabela[[col]])) {
      next()
    }
    tabela[[col]] <- gsub("[.*]", "", tabela[[col]]) %>% as.numeric()
  }
  for (col in c("obcina", "pokrajina", "regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}


# Klicanje zgornjih funkcij:

tabela1 <- uvozi.meritve(trenutno)
tabela2 <- uvozi.place(trenutno)
tabela3 <- uvozi.turizem(trenutno)
obcine <- uvozi.obcine()
View(tabela3)
# Urejanje podatkov: Podatke o ob�inah pretvorimo v podatke o regijah, ter izlo�ilo nepotrebne podatke.


regije <- subset(obcine, select=c("povrsina","prebivalci","naselja","regija"))
regije <- aggregate(cbind(povrsina, prebivalci, naselja) ~ regija, data=regije, FUN=sum)
#View(regije)

obcine <- subset(obcine, select=c("obcina","regija"))

zdruzena1 <- merge(obcine,tabela1, by = "obcina") %>% subset(select =c("regija","leto","vrsta.meritve","vrednost"))
zdruzena1 <- transform(zdruzena1, vrednost = as.integer(vrednost))
#zdruzena1 <- zdruzena1 %>% group_by(regija,leto,vrsta.meritve) %>% summarise_each(funs(sum))


zdruzena2 <- merge(obcine,tabela2, by = "obcina") %>% subset(select =c("regija","leto","povprecna.mesecna.bruto.placa"))
transform(zdruzena2, povprecna.mesecna.bruto.placa = as.numeric(povprecna.mesecna.bruto.placa))
#zdruzena2 <- zdruzena2 %>% group_by(regija,leto) %>% summarise_each(funs(mean))

zdruzena3 <- merge(obcine,tabela3, by = "obcina") %>% subset(select =c("regija","leto","parameter","vrednost"))
zdruzena3 <- transform(zdruzena3, vrednost = as.integer(vrednost))
zdruzena3 <- zdruzena3 %>% group_by(regija,leto,parameter) %>% summarise_each(funs(sum))


View(zdruzena1)
# View(zdruzena2)
View(zdruzena3)