# 2. Faza: uvoz podatkov
library(dplyr)


# Funkcija, ki uvozi podatke iz datoteke druzine.csv

uvozi.meritve <- function() {
  return(read.csv2("meritve.csv",
                   skip = 1,
                   header = FALSE,
                   col.names = c("OBÈINA", "VRSTA MERITVE", "LETO", "VREDNOST"),
                   na=c("", " "),
                   ))
}

tabela1 <- uvozi.meritve()
View(tabela1)

uvozi.place <- function() {
  return(read.csv2("place.csv",
                   skip = 3,
                   header = FALSE,
                   col.names = c("REGIJA","LETO","POVPREÈNA MESEÈNA BRUTO PLAÈA"),
                   na=c("", " ")
  ))
}

tabela2 <- uvozi.place()
View(tabela2)





