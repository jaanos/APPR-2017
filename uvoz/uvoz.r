# 2. faza: Uvoz podatkov
library(readr)
library(dplyr)
library(httr)

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
View(drzave)

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

# Funkcija, ki uvozi podatke o stevilu mladih (stevilo)
uvozi.mladi <- function() {
  data <- read_csv("podatki/yth_demo_010_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("leto", 'drzava', 'Skupno','enota', 'starost', 'stevilo', 'komentarji')
  izbris <- data$drzava == data$drzava[grep("^Euro", data$drzava, ignore.case=TRUE)]
  data <- data[!izbris,]
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  data$Skupno <- NULL
  data$enota <- NULL
  data$komentarji <- NULL
  return(data)
}

# Funkcija, ki uvozi podatke o izobrazenosti mladih (v odstotkih)
uvozi.izobrazba <- function() {
  data <- read_csv("podatki/yth_demo_040_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("leto", 'drzava', 'izobrazba', 'spol', 'starost', 'enota', 'stevilo', 'komentarji')
  izbris <- data$drzava == data$drzava[grep("^Euro", data$drzava, ignore.case=TRUE)]
  data <- data[!izbris,]
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  data$enota <- NULL
  data$komentarji <- NULL
  return(data)
}

# Funkcija, ki uvozi podatke o neformalnem izobrazevanju (v odstotkih)
uvozi.neformalno <- function() {
  data <- read_csv("podatki/yth_educ_060_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("leto", 'drzava', 'enota', 'spol', 'starost', 'stevilo', 'komentarji')
  izbris <- data$drzava == data$drzava[grep("^Euro", data$drzava, ignore.case=TRUE)]
  data <- data[!izbris,]
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  data$enota <- NULL
  data$komentarji <- NULL
  return(data)
}

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
  data$enota <- NULL
  data$komentarji <- NULL
  return(data)
}

# Funkcija, ki uvozi podatke o neaktivnih (nezaposlenih, nešolajočih se) (v odstotkih)
uvozi.neaktivni <- function() {
  data <- read_csv("podatki/yth_empl_160_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), skip=1, na=':')
  names(data) <- c("leto", 'drzava', 'spol', 'starost', 'zaposlen', 'Neaktivnost', 'izobrazba', 'enota', 'vrednost', 'komentarji')
  izbris <- data$drzava == data$drzava[grep("^Euro", data$drzava, ignore.case=TRUE)]
  data <- data[!izbris,]
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  data$enota <- NULL
  data$komentarji <- NULL
  data$zaposlen <- NULL
  data$Neaktivnost <- NULL
  data$izobrazba <- NULL
  return(data)
}

# Funkcija, ki uvozi podatke o vkljucenosti v aktivnosti znotraj religij in cerkva (v odstotkih)
uvozi.religija <- function() {
  data <- read_csv("podatki/yth_part_030_1_Data.csv",
                   locale = locale(encoding = "Windows-1250"), na=':')
  names(data) <- c("starost", 'drzava', 'enota', 'spol', 'leto', 'DA_NE', 'vrednost')
  izbris <- data$drzava == data$drzava[grep("^Euro", data$drzava, ignore.case=TRUE)]
  data <- data[!izbris,]
  data$drzava <- gsub(" \\(.*\\)$", "", data$drzava, ignore.case=TRUE)
  data$enota <- NULL
  data$DA_NE <- NULL
  return(data)
}

# Funkcija, ki uvozi podatke o prostovoljstvu
uvozi.prostovoljstvo <- function() {
  stran <- GET('http://appsso.eurostat.ec.europa.eu/nui/download?p=92c60fbc-4bf4-47ef-9757-17147c788370-1494233436416_&_=1494233696613') %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[1]] %>% html_table(dec = ",")
  colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
                        "ustanovitev", "pokrajina", "regija", "odcepitev")
  tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
  tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
  tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
  for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
    tabela[[col]] <- parse_number(tabela[[col]], na = "-", locale = sl)
  }
  for (col in c("obcina", "pokrajina", "regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}

prostovoljstvo <- uvozi.prostovoljstvo()
