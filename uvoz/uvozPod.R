# 2. Faza: uvoz podatkov

# Knjižnjice:
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
                      col.names = c("OBÈINA", "VRSTA MERITVE", "LETO", "VREDNOST"),
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
                      col.names = c("OBÈINA","LETO","POVPREÈNA MESEÈNA BRUTO PLAÈA"),
                      skip = 3,
                      header = FALSE,
                      na=c("", " ")
  )
  tabela <- tabela %>% fill(1) 
  vrstice.z.na <- apply(tabela, 1, function(x){any(is.na(x))})
  tabela <- tabela[!vrstice.z.na,]
  return(tabela)
}


# Klicanje zgornjih funkcij:

tabela1 <- uvozi.meritve(trenutno)
View(tabela1)

tabela2 <- uvozi.place(trenutno)
View(tabela2)







