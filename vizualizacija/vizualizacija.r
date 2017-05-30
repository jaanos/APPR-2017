# 3. faza: Vizualizacija podatkov
library(ggplot2)
library(dplyr)

# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid("http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/NUTS_2013_10M_SH.zip",
                             "NUTS_2013_10M_SH/data/NUTS_RG_10M_2013", encoding = "UTF-8") %>%
  pretvori.zemljevid() %>% filter(STAT_LEVL_ == 0) %>% filter(NUTS_ID == 'SI' | NUTS_ID == 'IT' | NUTS_ID == 'SE' | 
                                                                NUTS_ID == 'PL' | NUTS_ID == 'HR' | NUTS_ID == 'HU' | 
                                                                NUTS_ID == 'AT' | NUTS_ID == 'UK' | NUTS_ID == 'FR')

ggplot() + geom_polygon(data = zemljevid, aes(x = long, y = lat, group = group, color=NUTS_ID), color='red') +
  coord_map(xlim = c(-25, 40), ylim = c(32, 72))

#zemljevid$NUTS_ID <- factor(zemljevid$NUTS_ID, levels = levels(drzave$drzava))
#zemljevid <- pretvori.zemljevid(zemljevid)

# Izračunamo povprečno velikost družine
#povprecja <- druzine %>% group_by(obcina) %>%
#  summarise(povprecje = sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))

a <- ggplot(drzave %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                drzava == 'United Kingdom' | drzava == 'Italy' |
                                drzava == 'Slovenia' | drzava == 'Poland' |
                                drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=BDPpc, color = drzava) +
  geom_line() + ggtitle("BDP")
#print(a)

b <- ggplot(drzavljani %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                drzava == 'United Kingdom' | drzava == 'Italy' |
                                drzava == 'Slovenia' | drzava == 'Poland' |
                                drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=drzavljani, color = drzava) +
  geom_line() + ggtitle("Prebivalstvo")
#print(b)

c <- ggplot(mladi %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                    drzava == 'United Kingdom' | drzava == 'Italy' |
                                    drzava == 'Slovenia' | drzava == 'Poland' |
                                    drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=mladi, color = drzava, linetype= starost) +
  geom_line() + ggtitle("Mladi")
#print(c)

# as.numeric(date)

d <- ggplot(izobrazba %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                    drzava == 'United Kingdom' | drzava == 'Italy' |
                                    drzava == 'Slovenia' | drzava == 'Poland' |
                                    drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=izobrazba, color = drzava) +
  geom_line() + ggtitle("Izobrazba")
#print(d)

e <- ggplot(neformalno %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                               drzava == 'United Kingdom' | drzava == 'Italy' |
                               drzava == 'Slovenia' | drzava == 'Poland' |
                               drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=neformalno, color = drzava, linetype=spol) +
  geom_line() + ggtitle("Neformalno izobraževanje")
#print(e)

f <- ggplot(zaposlenost %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                               drzava == 'United Kingdom' | drzava == 'Italy' |
                               drzava == 'Slovenia' | drzava == 'Poland' |
                               drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=zaposlenost, color = drzava) +
  geom_line() + ggtitle("Zaposlenost mladih")
#print(f)

g <- ggplot(neaktivni %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                     drzava == 'United Kingdom' | drzava == 'Italy' |
                                     drzava == 'Slovenia' | drzava == 'Poland' |
                                     drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=neaktivni, color = drzava) +
  geom_line() + ggtitle("Neaktivni mladi")
#print(g)

h <- ggplot(religija %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                     drzava == 'United Kingdom' | drzava == 'Italy' |
                                     drzava == 'Slovenia' | drzava == 'Poland' |
                                     drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=drzava, y=religija, color=spol, shape = starost) +
  geom_point() + ggtitle("Versko udejstvovanje")
#print(h)

i <- ggplot(prostovoljstvo %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                     drzava == 'United Kingdom' | drzava == 'Italy' |
                                     drzava == 'Slovenia' | drzava == 'Poland' |
                                     drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=drzava, y = prostovoljstvo, color = starost, shape = spol) +
  geom_point() + ggtitle("Prostovoljstvo")
#print(i)

graf1 <- merge(drzave, drzavljani)
j <- ggplot(graf1 %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                        drzava == 'United Kingdom' | drzava == 'Italy' |
                                        drzava == 'Slovenia' | drzava == 'Poland' |
                                        drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=BDPpc, y = drzavljani, color = drzava, size = leto) +
  geom_point() + ggtitle("graf1")
#print(j)

graf3 <- merge(izobrazba, religija)
l <- ggplot(graf3 %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                               drzava == 'United Kingdom' | drzava == 'Italy' |
                               drzava == 'Slovenia' | drzava == 'Poland' |
                               drzava == 'Austria' | drzava == 'Croatia') %>% filter(leto==2006)) +
  aes(x=izobrazba, y = religija, color = drzava, shape = spol) +
  geom_point() + ggtitle("graf3")
#print(l)

