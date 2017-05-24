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
  aes(x=leto, y=vrednost, color = drzava) +
  geom_line() + ggtitle("BDP")
print(a)

b <- ggplot(drzavljani %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                drzava == 'United Kingdom' | drzava == 'Italy' |
                                drzava == 'Slovenia' | drzava == 'Poland' |
                                drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=stevilo, color = drzava) +
  geom_line() + ggtitle("Prebivalstvo")
print(b)

c <- ggplot(mladi %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                    drzava == 'United Kingdom' | drzava == 'Italy' |
                                    drzava == 'Slovenia' | drzava == 'Poland' |
                                    drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=stevilo, color = drzava) +
  geom_line() + ggtitle("Mladi")
print(c)

d <- ggplot(izobrazba %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                    drzava == 'United Kingdom' | drzava == 'Italy' |
                                    drzava == 'Slovenia' | drzava == 'Poland' |
                                    drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=stevilo, color = drzava) +
  geom_point() + ggtitle("Izobrazba")
print(d)

e <- ggplot(neformalno %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                               drzava == 'United Kingdom' | drzava == 'Italy' |
                               drzava == 'Slovenia' | drzava == 'Poland' |
                               drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=stevilo, color = spol) +
  geom_line() + ggtitle("Neformalno izobraževanje")
print(e)

f <- ggplot(zaposlenost %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                               drzava == 'United Kingdom' | drzava == 'Italy' |
                               drzava == 'Slovenia' | drzava == 'Poland' |
                               drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=stevilo, color = izobrazba) +
  geom_line() + ggtitle("Zaposlenost mladih")
print(f)

g <- ggplot(neaktivni %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                     drzava == 'United Kingdom' | drzava == 'Italy' |
                                     drzava == 'Slovenia' | drzava == 'Poland' |
                                     drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=leto, y=vrednost, color = drzava) +
  geom_line() + ggtitle("Neaktivni mladih")
print(g)

h <- ggplot(religija %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                     drzava == 'United Kingdom' | drzava == 'Italy' |
                                     drzava == 'Slovenia' | drzava == 'Poland' |
                                     drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=drzava, y=vrednost, color = spol) +
  geom_point() + ggtitle("Versko udejstvovanje")
print(h)

i <- ggplot(prostovoljstvo %>% filter(drzava == 'Hungary' | drzava == 'France' | drzava == 'Sweden' |
                                     drzava == 'United Kingdom' | drzava == 'Italy' |
                                     drzava == 'Slovenia' | drzava == 'Poland' |
                                     drzava == 'Austria' | drzava == 'Croatia')) +
  aes(x=drzava, y=od16do19, color = spol) +
  geom_point() + ggtitle("Prostovoljstvo")
print(i)