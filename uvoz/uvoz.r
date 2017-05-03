stolpci<-c("OBČINE","VRSTA PRIREDITVE","VRSTA PRODUKCIJE","LETO","ŠTEVILO PRIREDITEV")
#uvozimo podatke iz /podatki
uvozi.prireditve <- function() {
return(read.csv2("podatki/podatki2.csv",header=FALSE,encoding="Windows-1250",skip=2, col.names=stolpci,nrows=7248))
}

#urejamo podatke
tabela<-uvozi.prireditve()
tabela$ŠTEVILO.PRIREDITEV[tabela$ŠTEVILO.PRIREDITEV =="-"] <-NA
tabela$OBČINE[tabela$OBČINE ==" "] <-NA
tabela$OBČINE<-na.locf(tabela$OBČINE)
tabela$VRSTA.PRIREDITVE[tabela$VRSTA.PRIREDITVE ==" "] <-NA
tabela$VRSTA.PRIREDITVE<-na.locf(tabela$VRSTA.PRIREDITVE)
tabela$VRSTA.PRODUKCIJE[tabela$VRSTA.PRODUKCIJE ==" "] <-NA
tabela$VRSTA.PRODUKCIJE<-na.locf(tabela$VRSTA.PRODUKCIJE)
require(zoo)
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
