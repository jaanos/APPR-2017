# 3. faza: Vizualizacija podatkov
library(ggplot2)
library(dplyr)
# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip",
                             "OB/OB", encoding = "Windows-1250")
levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels = levels(drzave$drzava))
zemljevid <- pretvori.zemljevid(zemljevid)

# Izračunamo povprečno velikost družine
#povprecja <- druzine %>% group_by(obcina) %>%
#  summarise(povprecje = sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))

g <- ggplot(drzave) + aes(x=leto, y=vrednost, color = drzava) +
  geom_line() + ggtitle("BDP")
# ggplot(drzave %>% filter(drzava == 'Slovenia'))
print(g)
