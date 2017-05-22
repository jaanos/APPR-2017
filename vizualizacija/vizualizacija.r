# 3. faza: Vizualizacija podatkov
#source("uvoz.r")
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

# Uvozimo zemljevid.
Svet <- map_data("world")
head(Svet)

slo <- uv