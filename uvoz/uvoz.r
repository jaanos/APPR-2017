# 2. faza: Uvoz podatkov:

# Knjižnjice:
Encoding("UTF-8")
library(dplyr)
library(rvest)
library(dplyr)
library(reader)
library(stats)
library(tidyr)

# Nastavitev relativne poti do csv datotek:

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
setwd('..')
trenutno = setwd(Sys.getenv("HOME"));


# Funkciji, ki uvozita podatke iz datoteke druzine csv:

uvozi.meritve <- function(trenutno) {
  fpot = file.path(trenutno, 'podatki', "meritve.csv");
  tabela <- read.csv2(fpot,
                      skip = 1,
                      header = FALSE,
                      col.names = c("OB?INA", "VRSTA MERITVE", "LETO", "VREDNOST"),
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
                      col.names = c("OB?INA","LETO","POVPRE?NA MESE?NA BRUTO PLA?A"),
                      skip = 3,
                      header = FALSE,
                      na=c("", " ")
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
                      na=c("", " "),
                      col.names = c("OB?INA", "LETO", "PARAMETER" , "VREDNOST"),
                      header = FALSE
  )
  tabela <- tabela %>% fill(1:2)
  vrstice.z.na <- apply(tabela, 1, function(x){any(is.na(x))})
  tabela <- tabela[!vrstice.z.na,]
  return(tabela)  
}


# Klicanje zgornjih funkcij:

tabela1 <- uvozi.meritve(trenutno)
#View(tabela1)

tabela2 <- uvozi.place(trenutno)
#View(tabela2)

tabela3 <- uvozi.turizem(trenutno)
#View(tabela3)



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


# Klicanje funkcije:
obcine <- uvozi.obcine()
#View(obcine)

