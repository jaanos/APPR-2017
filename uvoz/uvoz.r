# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark = ",", grouping_mark = ".")

# Funkcija, ki uvozi občine iz Wikipedije
uvozi.obcine <- function() {
  link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
  stran <- html_session(link) %>% read_html()
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

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function(obcine) {
  data <- read_csv2("podatki/druzine.csv", col_names = c("obcina", 1:4),
                    locale = locale(encoding = "Windows-1250"))
  data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
    strapplyc("([^ ]+)") %>% sapply(paste, collapse = " ") %>% unlist()
  data$obcina[data$obcina == "Sveti Jurij"] <- "Sveti Jurij ob Ščavnici"
  data <- data %>% melt(id.vars = "obcina", variable.name = "velikost.druzine",
                        value.name = "stevilo.druzin")
  data$velikost.druzine <- parse_number(data$velikost.druzine)
  data$obcina <- factor(data$obcina, levels = obcine)
  return(data)
}

# Zapišimo podatke v razpredelnico obcine
obcine <- uvozi.obcine()

# Zapišimo podatke v razpredelnico druzine.
druzine <- uvozi.druzine(levels(obcine$obcina))

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.




worldbank <- read.csv("podatki/uvoz.r", header=TRUE,
                      col.names=c("serija", "koda serije", "drzava", "koda drzave", 
                                  "2000", "2001", "2002", "2003", "2004", "2005",
                                  "2006", "2007", "2008", "2009", "2010", "2011",
                                  "2012", "2013", "2014", "2015"),
                      na.strings="..")

# Pojavnost tuberkuloze
tuberkuloza <- read.csv("podatki/who-tuberculosis", header=TRUE,
                        col.names=c("drzava", "leto", "pojavnost"))

# Pojavnost kolere
kolera <- read.csv("podatki/who-cholera", header=TRUE,
                   col.names=c("drzava", "leto", "pojavnost"))

# Pojavnost prirojenega sifilisa
sifilis <- read.csv("podatki/who-sifilis", header=TRUE, 
                    col.names = c("drzava","vir", "leto", "pojavnost"))

# Poraba alkohola (v litrih čistega alkohola) na osebo (15+)
alkohol <- read.csv("podatki/who-alcohol", skip=1, header=TRUE,
                    col.names=c("drzava", "vir", "tip pijace", "2015", "2014", "2013",
                                "2012", "2011", "2010", "2009", "2008", "2007", "2006",
                                "2005", "2004", "2003", "2002", "2001", "2000"), 
                    na.strings="")

# Razširjenost kajenja tobačnih izdelkov 15+
tobak <- read.csv("podatki/who-tabacco", skip=1, header=TRUE,
                  col.names=c("drzava","leto", "moski", "zenske"), na.strings="")
