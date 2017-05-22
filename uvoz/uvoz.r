# 2. faza: Uvoz podatkov
# drzave, drzavljani, mladi, izobrazba, neformalno, zaposlenost, neaktivni, religija, prostovoljstvo
library(rvest)
library(gsubfn)
library(readr)
library(dplyr)

# Funkcija, ki uvozi podatke o BDP-jih držav (v EUR na osebo)
uvozi.drzave <- function() {
  data <- read_csv("podatki/nama_10_pc_1_Data.csv",
                    locale = locale(encoding = "Windows-1250"))
  names(data) <- c("leto", 'drzava', 'enota', 'BDP', 'vrednost')
  izbris <- data$drzava == data$drzava[grep("^Euro", data$drzava, ignore.case=TRUE)]
  data <- data[!izbris,]
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  izbris2 <- data$enota == data$enota[grep("Current prices, euro per capita", data$enota, ignore.case=TRUE)]
  data <- data[izbris2,]
  izbris3 <- data$BDP == data$BDP[grep("Gross domestic product at market prices", data$BDP, ignore.case=TRUE)]
  data <- data[izbris3,]
  data$enota <- NULL
  data$BDP <- NULL
  return(data)
}
drzave <- uvozi.drzave()
drzave$vrednost <- parse_number(drzave$vrednost, na=c(NA, ":"))
drzave <- na.omit(drzave)

# Funkcija, ki uvozi podatke o drzavljanih (v tisoč)
uvozi.drzavljani <- function() {
  data <- read_csv("podatki/nama_10_pe_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"))
  names(data) <- c("leto", 'drzava', 'enota', 'Skupno', 'stevilo')
  izbris <- data$drzava == data$drzava[grep("^Euro", data$drzava, ignore.case=TRUE)]
  data <- data[!izbris,]
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  izbris2 <- data$enota == data$enota[grep("Thousand persons", data$enota, ignore.case=TRUE)]
  data <- data[izbris2,]
  data$enota <- NULL
  data$Skupno <- NULL
  return(data)
}
drzavljani <- uvozi.drzavljani()
drzavljani$stevilo <- parse_number(drzavljani$stevilo, na=c(NA, ":"))
drzavljani <- na.omit(drzavljani)

# Funkcija, ki uvozi podatke o stevilu mladih (stevilo)
uvozi.mladi <- function() {
  data <- read_csv("podatki/yth_demo_010_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("leto", 'drzava', 'Skupno','enota', 'starost', 'stevilo', 'komentarji')
  izbris <- data$drzava == data$drzava[grep("^Euro", data$drzava, ignore.case=TRUE)]
  data <- data[!izbris,]
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  data$starost <- gsub("From 15 to 29 years", "15 do 29", data$starost, ignore.case=TRUE)
  data$starost <- gsub("Less than 15 years", "do 15", data$starost, ignore.case=TRUE)
  data$Skupno <- NULL
  data$enota <- NULL
  data$komentarji <- NULL
  return(data)
}
mladi <- uvozi.mladi()
mladi$stevilo <- parse_number(mladi$stevilo, na=c(NA, ":"))
mladi <- na.omit(mladi)

# Funkcija, ki uvozi podatke o izobrazenosti mladih (v odstotkih, ženske)
uvozi.izobrazba <- function() {
  data <- read_csv("podatki/yth_demo_040_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("leto", 'drzava', 'izobrazba', 'spol', 'starost', 'enota', 'stevilo', 'komentarji')
  izbris <- data$drzava == data$drzava[grep("^Euro", data$drzava, ignore.case=TRUE)]
  data <- data[!izbris,]
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  data$enota <- NULL
  data$spol <- NULL
  data$starost <- NULL
  data$izobrazba <- NULL
  data$komentarji <- NULL
  return(data)
}
izobrazba <- uvozi.izobrazba()
izobrazba$stevilo <- parse_number(izobrazba$stevilo, na=c(NA, ":"))
izobrazba <- na.omit(izobrazba)

# Funkcija, ki uvozi podatke o neformalnem izobrazevanju (v odstotkih, stari 15-24)
uvozi.neformalno <- function() {
  data <- read_csv("podatki/yth_educ_060_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("leto", 'drzava', 'enota', 'spol', 'starost', 'stevilo', 'komentarji')
  izbris <- data$drzava == data$drzava[grep("^Euro", data$drzava, ignore.case=TRUE)]
  data <- data[!izbris,]
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  izbris2 <- data$spol != data$spol[grep("Total", data$spol, ignore.case=TRUE)]
  data <- data[izbris2,]
  data$enota <- NULL
  data$komentarji <- NULL
  data$starost <- NULL
  return(data)
}
neformalno <- uvozi.neformalno()
neformalno$stevilo <- parse_number(neformalno$stevilo, na=c(NA, ":"))
neformalno <- na.omit(neformalno)

# Funkcija, ki uvozi podatke o zaposlenosti (v odstotkih)
uvozi.zaposlenost <- function() {
  data <- read_csv("podatki/yth_empl_010_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), skip=1, na=':')
  names(data) <- c("leto", 'drzava', 'spol', 'starost', 'enota', 'izobrazba', 'stevilo', 'komentarji')
  izbris <- data$drzava == data$drzava[grep("^Euro", data$drzava, ignore.case=TRUE)]
  data <- data[!izbris,]
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  izbris2 <- data$enota != data$enota[grep("Percentage", data$enota, ignore.case=TRUE)]
  data <- data[izbris2,]
  izbris3 <- data$spol != data$spol[grep("Total", data$spol, ignore.case=TRUE)]
  data <- data[izbris3,]
  data$starost <- gsub("[^0-9]*([0-9]+)[^0-9]*([0-9]+)[^0-9]*", "\\1 do \\2", data$starost, ignore.case=TRUE)
  izbris4 <- data$izobrazba != data$izobrazba[grep("^((All)|(No))", data$izobrazba, ignore.case=TRUE)]
  data <- data[izbris4,]
  data$starost <- data$starost[!(data$starost == '15 do 24' | data$starost == '15 do 29' | data$starost == '20 do 29')]
  data$izobrazba <- gsub(".+\\(levels ([0-9])[^0-9]+([0-9])\\)$", "\\1-\\2", data$izobrazba, ignore.case=TRUE)
  data$enota <- NULL
  data$komentarji <- NULL
  return(data)
}
zaposlenost <- uvozi.zaposlenost()
zaposlenost$stevilo <- parse_number(zaposlenost$stevilo, na=c(NA, ":"))
zaposlenost <- na.omit(zaposlenost)

# Funkcija, ki uvozi podatke o neaktivnih (nezaposlenih, nešolajočih se, starih 15-19) (v odstotkih)
uvozi.neaktivni <- function() {
  data <- read_csv("podatki/yth_empl_160_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), skip=1, na=':')
  names(data) <- c("leto", 'drzava', 'spol', 'starost', 'zaposlen', 'Neaktivnost', 'izobrazba', 'enota', 'vrednost', 'komentarji')
  izbris <- data$drzava == data$drzava[grep("^Euro", data$drzava, ignore.case=TRUE)]
  data <- data[!izbris,]
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  data$enota <- NULL
  data$starost <- NULL
  data$komentarji <- NULL
  data$zaposlen <- NULL
  data$spol <- NULL
  data$Neaktivnost <- NULL
  data$izobrazba <- NULL
  return(data)
}
neaktivni <- uvozi.neaktivni()
neaktivni$vrednost <- parse_number(neaktivni$vrednost, na=c(NA, ":"))
neaktivni <- na.omit(neaktivni)

# Funkcija, ki uvozi podatke o vkljucenosti v aktivnosti znotraj religij in cerkva (v odstotkih, 2006)
uvozi.religija <- function() {
  data <- read_csv("podatki/yth_part_030_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("starost", 'drzava', 'enota', 'spol', 'leto', 'DA_NE', 'vrednost')
  izbris <- data$drzava == data$drzava[grep("^Euro", data$drzava, ignore.case=TRUE)]
  data <- data[!izbris,]
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  izbris2 <- data$spol != data$spol[grep("Total", data$spol, ignore.case=TRUE)]
  data <- data[izbris2,]
  data$starost <- gsub("[^0-9]*([0-9]+)[^0-9]*([0-9]+)[^0-9]*", "\\1 - \\2", data$starost, ignore.case=TRUE)
  data$enota <- NULL
  data$DA_NE <- NULL
  data$leto <- NULL
  return(data)
}
religija <- uvozi.religija()
religija$vrednost <- parse_number(religija$vrednost, na=c(NA, ":"))
religija <- na.omit(religija)

# Funkcija, ki uvozi podatke o prostovoljstvu (v odstotkih za 2006)
uvozi.prostovoljstvo <- function() {
  stran <- 'podatki/yth_volunt_010.html' %>% read_html()
  moski <- stran %>% html_nodes(xpath="//table") %>%
    .[[2]] %>% html_table(dec = ".")
  colnames(moski) <- c("drzava", "od16do29", "od20do29", "od16do19", "od16do24",
                        "od20do24", "od25do29")
  izbris <- moski$drzava == moski$drzava[grep("^Euro", moski$drzava, ignore.case=TRUE)]
  moski <- moski[!izbris,]
  moski <- moski[-28,]
  moski$drzava <- gsub(" \\(.*\\)$", "", moski$drzava, ignore.case=TRUE)
  moski$spol <- 'males'
  zenske <- stran %>% html_nodes(xpath="//table") %>%
    .[[3]] %>% html_table(dec = ".")
  colnames(zenske) <- c("drzava", "od16do29", "od20do29", "od16do19", "od16do24",
                        "od20do24", "od25do29")
  izbris <- zenske$drzava == zenske$drzava[grep("^Euro", zenske$drzava, ignore.case=TRUE)]
  zenske <- zenske[!izbris,]
  zenske <- zenske[-28,]
  zenske$drzava <- gsub(" \\(.*\\)$", "", zenske$drzava, ignore.case=TRUE)
  zenske$spol <- 'females'
  moski$od16do29 <- NULL
  moski$od20do29 <- NULL
  moski$od16do24 <- NULL
  zenske$od16do29 <- NULL
  zenske$od20do29 <- NULL
  zenske$od16do24 <- NULL
  tabela <- rbind(moski, zenske)
  for (col in c("od16do19", "od20do24", "od25do29")) {
    tabela[[col]] <- parse_number(tabela[[col]], na = "-")
  }
  for (col in c("drzava", "spol")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  a <- tabela[c(1,2,5)]
  a$starost <- '16-19'
  colnames(a) <- c('drzava', 'stevilo', 'spol', 'starost')
  b <- tabela[c(1,3,5)]
  b$starost <- '20-24'
  colnames(b) <- c('drzava', 'stevilo', 'spol', 'starost')
  c <- tabela[c(1,4,5)]
  c$starost <- '25-29'
  colnames(c) <- c('drzava', 'stevilo', 'spol', 'starost')
  tabela <- rbind(a, b, c)
  return(tabela)
}
prostovoljstvo <- uvozi.prostovoljstvo()
