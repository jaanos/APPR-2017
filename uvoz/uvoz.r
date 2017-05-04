library(tidyr)
uvozi.prireditve <- function() {
  stolpci <- c("OBCINE","VRSTA.PRIREDITVE","VRSTA.PRODUKCIJE","LETO","STEVILO.PRIREDITEV")
  tab <- read_csv2("podatki/podatki2.csv", locale = locale(encoding = "Windows-1250"),
                    na = c("", "-"), skip = 2, col_names = stolpci, n_max = 7248) %>%
    fill(1:4) %>% drop_na(STEVILO.PRIREDITEV)
  return(tab)
}

#urejamo podatke
tabela<-subset(tabela, tabela$LETO>2000 & tabela$LETO <=2020)
#naredimo ločeni tabeli, v kateri bodo števila obiskovalcev
tabela2<-subset(tabela, VRSTA.PRODUKCIJE == "..Število obiskovalcev (otroci in mladina)")
tabela2$VRSTA.PRODUKCIJE<-NULL
colnames(tabela2)<-c("OBČINE","VRSTA.PRIREDITVE","LETO","ŠTEVILO OBISKOVALCEV (otrok in mladine)")
tabela<-subset(tabela, VRSTA.PRODUKCIJE!="..Število obiskovalcev (otroci in mladina)")
tabela3<-subset(tabela, VRSTA.PRODUKCIJE == "Število obiskovalcev - SKUPAJ")
tabela<-subset(tabela, VRSTA.PRODUKCIJE!="Število obiskovalcev - SKUPAJ")
tabela3$VRSTA.PRODUKCIJE<-NULL
colnames(tabela3)<-c("OBČINE","VRSTA.PRIREDITVE","LETO","ŠTEVILO OBISKOVALCEV SKUPAJ")

#graf števila prireditev instrumentalnih koncertov v Sloveniji po letih:
graf<-ggplot(tabela %>% filter(OBČINE=="SLOVENIJA"&VRSTA.PRIREDITVE=="Instrumentalni koncerti"&VRSTA.PRODUKCIJE=="Vse prireditve - SKUPAJ"))+aes(x=LETO, y=ŠTEVILO.PRIREDITEV)+geom_point()
print(graf)
