# 2. Faza: uvoz podatkov
library(dplyr)


# Funkcija, ki uvozi podatke iz datoteke druzine.csv

uvozi.poslovanje <- function() {
  return(read.csv2("poslovanjePod.csv",
                   skip = 1,
                   header = FALSE,
                   col.names = c("SKD DEJAVNOST", "VRSTA KAZALNIKA", "ÈETRTLETJE", "VREDNOST"),
                   na=c("", " ")
                   ))
}


View(tabela1)


