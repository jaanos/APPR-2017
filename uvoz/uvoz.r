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
  names(data) <- c("leto", 'drzava', 'enota', 'BDP', 'BDPpc')
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
#drzave <- uvozi.drzave()
#drzave$BDPpc <- parse_number(drzave$BDPpc, na=c(NA, ":"))
#drzave <- na.omit(drzave)

# Funkcija, ki uvozi podatke o drzavljanih (v tisoč)
uvozi.drzavljani <- function() {
  data <- read_csv("podatki/nama_10_pe_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"))
  names(data) <- c("leto", 'drzava', 'enota', 'Skupno', 'drzavljani')
  izbris <- data$drzava == data$drzava[grep("^Euro", data$drzava, ignore.case=TRUE)]
  data <- data[!izbris,]
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  izbris2 <- data$enota == data$enota[grep("Thousand persons", data$enota, ignore.case=TRUE)]
  data <- data[izbris2,]
  data$enota <- NULL
  data$Skupno <- NULL
  return(data)
}
#drzavljani <- uvozi.drzavljani()
#drzavljani$drzavljani <- parse_number(drzavljani$drzavljani, na=c(NA, ":"))
#drzavljani <- na.omit(drzavljani)

# Funkcija, ki uvozi podatke o stevilu mladih (stevilo)
uvozi.mladi <- function() {
  data <- read_csv("podatki/yth_demo_010_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("leto", 'drzava', 'Skupno','enota', 'starost', 'mladi', 'komentarji')
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
#mladi <- uvozi.mladi()
#mladi$mladi <- parse_number(mladi$mladi, na=c(NA, ":"))
#mladi <- na.omit(mladi)

# Funkcija, ki uvozi podatke o izobrazenosti mladih (v odstotkih, ženske)
uvozi.izobrazba <- function() {
  data <- read_csv("podatki/yth_demo_040_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("leto", 'drzava', 'izobrazba', 'spol', 'starost', 'enota', 'izobrazba', 'komentarji')
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
#izobrazba <- uvozi.izobrazba()
#izobrazba$izobrazba <- parse_number(izobrazba$izobrazba, na=c(NA, ":"))
#izobrazba <- na.omit(izobrazba)

# Funkcija, ki uvozi podatke o neformalnem izobrazevanju (v odstotkih, stari 15-24)
uvozi.neformalno <- function() {
  data <- read_csv("podatki/yth_educ_060_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("leto", 'drzava', 'enota', 'spol', 'starost', 'neformalno', 'komentarji')
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
#neformalno <- uvozi.neformalno()
#neformalno$neformalno <- parse_number(neformalno$neformalno, na=c(NA, ":"))
#neformalno <- na.omit(neformalno)

# Funkcija, ki uvozi podatke o zaposlenosti (v odstotkih)
uvozi.zaposlenost <- function() {
  data <- read_csv("podatki/yth_empl_010_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), skip=1, na=':')
  names(data) <- c("leto", 'drzava', 'spol', 'starost', 'enota', 'izobrazenost', 'zaposlenost', 'komentarji')
  izbris <- data$drzava == data$drzava[grep("^Euro", data$drzava, ignore.case=TRUE)]
  data <- data[!izbris,]
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  izbris2 <- data$enota != data$enota[grep("Percentage", data$enota, ignore.case=TRUE)]
  data <- data[izbris2,]
  izbris3 <- data$spol != data$spol[grep("Total", data$spol, ignore.case=TRUE)]
  data <- data[izbris3,]
  data$starost <- gsub("[^0-9]*([0-9]+)[^0-9]*([0-9]+)[^0-9]*", "\\1 do \\2", data$starost, ignore.case=TRUE)
  izbris4 <- data$izobrazenost != data$izobrazenost[grep("^((All)|(No))", data$izobrazenost, ignore.case=TRUE)]
  data <- data[izbris4,]
  data$starost <- data$starost[!(data$starost == '15 do 24' | data$starost == '15 do 29' | data$starost == '20 do 29')]
  data$izobrazenost <- gsub(".+\\(levels ([0-9])[^0-9]+([0-9])\\)$", "\\1-\\2", data$izobrazenost, ignore.case=TRUE)
  data$enota <- NULL
  data$komentarji <- NULL
  return(data)
}
zaposlenost <- uvozi.zaposlenost()
zaposlenost$zaposlenost <- parse_number(zaposlenost$zaposlenost, na=c(NA, ":"))
zaposlenost <- na.omit(zaposlenost)

# Funkcija, ki uvozi podatke o neaktivnih (nezaposlenih, nešolajočih se, starih 15-19) (v odstotkih)
uvozi.neaktivni <- function() {
  data <- read_csv("podatki/yth_empl_160_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), skip=1, na=':')
  names(data) <- c("leto", 'drzava', 'spol', 'starost', 'zaposlen', 'Neaktivnost', 'izobrazba', 'enota', 'neaktivni', 'komentarji')
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
#neaktivni <- uvozi.neaktivni()
#neaktivni$neaktivni <- parse_number(neaktivni$neaktivni, na=c(NA, ":"))
#neaktivni <- na.omit(neaktivni)

# Funkcija, ki uvozi podatke o vkljucenosti v aktivnosti znotraj religij in cerkva (v odstotkih, 2006)
uvozi.religija <- function() {
  data <- read_csv("podatki/yth_part_030_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("starost", 'drzava', 'enota', 'spol', 'leto', 'DA_NE', 'religija')
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
#religija <- uvozi.religija()
#religija$religija <- parse_number(religija$religija, na=c(NA, ":"))
#religija <- na.omit(religija)

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
  names(a)[2] <- 'prostovoljstvo'
  a$starost <- '16-19'
  b <- tabela[c(1,3,5)]
  names(b)[2] <- 'prostovoljstvo'
  b$starost <- '20-24'
  c <- tabela[c(1,4,5)]
  names(c)[2] <- 'prostovoljstvo'
  c$starost <- '25-29'
  tabela <- rbind(a, b, c)
  return(tabela)
}
#prostovoljstvo <- uvozi.prostovoljstvo()

#write.csv(drzave,"podatki/urejeni_podatki/Drzave.csv",row.names=FALSE)
#write.csv(drzavljani,"podatki/urejeni_podatki/Drzavljani.csv",row.names=FALSE)
#write.csv(mladi,"podatki/urejeni_podatki/Mladi.csv",row.names=FALSE)
#write.csv(izobrazba,"podatki/urejeni_podatki/Izobrazba.csv",row.names=FALSE)
#write.csv(neformalno,"podatki/urejeni_podatki/Neformalno.csv",row.names=FALSE)
write.csv(zaposlenost,"podatki/urejeni_podatki/Zaposlenost.csv",row.names=FALSE)
#write.csv(neaktivni,"podatki/urejeni_podatki/Neaktivni.csv",row.names=FALSE)
#write.csv(prostovoljstvo,"podatki/urejeni_podatki/Prostovoljstvo.csv",row.names=FALSE)
#write.csv(religija,"podatki/urejeni_podatki/Religija.csv",row.names=FALSE)

drzave <- read_csv("podatki/urejeni_podatki/Drzave.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
drzavljani <- read_csv("podatki/urejeni_podatki/Drzavljani.csv",
                       locale = locale(encoding = "Windows-1250"), na=':')
mladi <- read_csv("podatki/urejeni_podatki/Mladi.csv",
                  locale = locale(encoding = "Windows-1250"), na=':')
izobrazba <- read_csv("podatki/urejeni_podatki/Izobrazba.csv",
                      locale = locale(encoding = "Windows-1250"), na=':')
neformalno <- read_csv("podatki/urejeni_podatki/Neformalno.csv",
                       locale = locale(encoding = "Windows-1250"), na=':')
zaposlenost <- read_csv("podatki/urejeni_podatki/Zaposlenost.csv",
                        locale = locale(encoding = "Windows-1250"), na=':')
neaktivni <- read_csv("podatki/urejeni_podatki/Neaktivni.csv",
                      locale = locale(encoding = "Windows-1250"), na=':')
prostovoljstvo <- read_csv("podatki/urejeni_podatki/Prostovoljstvo.csv",
                           locale = locale(encoding = "Windows-1250"), na=':')
religija <- read_csv("podatki/urejeni_podatki/Religija.csv",
                     locale = locale(encoding = "Windows-1250"), na=':')

# primerjave
graf1 <- merge(drzave, drzavljani)
j <- ggplot(graf1 %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                               drzava == 'United Kingdom' | drzava == 'Italy' |
                               drzava == 'Slovenia' | drzava == 'Poland' |
                               drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=stevilo, y = stevilo, color = drzava, shape = leto) +
  geom_point() + ggtitle("graf1")
print(j)