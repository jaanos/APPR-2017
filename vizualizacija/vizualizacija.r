# 3. faza: Vizualizacija podatkov
#source("uvoz.r")
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

# Uvozimo zemljevid.

slo <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip", "SVN_adm1") %>% pretvori.zemljevid()
ggplot() + geom_polygon(data = slo, aes(x = long, y = lat, group = group), color = "black")
