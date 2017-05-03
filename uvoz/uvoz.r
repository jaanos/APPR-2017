# 2. faza: Uvoz podatkov
library(readr)
sl <- locale("sl", decimal_mark = ",", grouping_mark = ".")

# .html datoteke uvežene iz Eurostata
z <- grep('\\.html$', d)
podatki <- d[z]

html.datoteka <- paste('podatki/', podatki[1], sep='')

# Funkcija, ki uvozi podatke o BDP-jih držav
uvozi.drzave <- function() {
  data <- read_csv("podatki/nama_10_pc_1_Data.csv",
                    locale = locale(encoding = "Windows-1250"))
  names(data) <- c("leto", 'drzava', 'enota', 'BDP', 'vrednost')
  return(data)
}
View(uvozi.drzave())

# Funkcija, ki uvozi podatke o drzavljanih
uvozi.drzavljani <- function() {
  data <- read_csv("podatki/nama_10_pe_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"))
  names(data) <- c("leto", 'drzava', 'enota', 'Skupno', 'stevilo')
  return(data)
}
View(uvozi.drzavljani())

# Funkcija, ki uvozi podatke o stevilu mladih
uvozi.mladi <- function() {
  data <- read_csv("podatki/yth_demo_010_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"))
  names(data) <- c("leto", 'drzava', 'Skupno','enota', 'starost', 'stevilo')
  return(data)
}
View(uvozi.mladi())

# Funkcija, ki uvozi podatke o izobrazenosti mladih
uvozi.izobrazba <- function() {
  data <- read_csv("podatki/yth_demo_040_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"))
  names(data) <- c("leto", 'drzava', 'izobrazba', 'spol', 'starost', 'enota', 'stevilo')
  return(data)
}
View(uvozi.izobrazba())

# Funkcija, ki uvozi podatke o neformalnem izobrazevanju
uvozi.neformalno <- function() {
  data <- read_csv("podatki/yth_educ_060_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"))
  names(data) <- c("leto", 'drzava', 'enota', 'spol', 'starost', 'stevilo')
  return(data)
}
View(uvozi.neformalno())

# Funkcija, ki uvozi podatke o zaposlenosti
uvozi.zaposlenost <- function() {
  data <- read_csv("podatki/yth_empl_010_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"))
  names(data) <- c("leto", 'drzava', 'spol', 'starost', 'enota', 'izobrazba', 'stevilo')
  return(data)
}
View(uvozi.zaposlenost())

# Funkcija, ki uvozi podatke o neaktivnih
uvozi.neaktivni <- function() {
  data <- read_csv("podatki/yth_empl_160_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"))
  names(data) <- c("leto", 'drzava', 'spol', 'starost', 'delovni status', 'Neaktivnost', 'izobrazba', 'enota', 'vrednost')
  return(data)
}
View(uvozi.neaktivni())

# Funkcija, ki uvozi podatke o vkljucenosti v aktivnosti znotraj religij in cerkva
uvozi.religija <- function() {
  data <- read_csv("podatki/yth_part_030_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"))
  names(data) <- c("starost", 'drzava', 'enota', 'spol', 'leto', 'DA_NE', 'vrednost')
  return(data)
}
View(uvozi.religija())

# Funkcija, ki uvozi podatke o prostovoljstvu
uvozi.prostovoljstvo <- function() {
  # podatki/yth_volunt_010.html
}
