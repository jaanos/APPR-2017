# 4. faza: Analiza podatkov

podatki <- velika_tabela %>% transmute(drzava, drzavljani, mladi,
                                nekaj = zaposlenost+izobrazba+neformalno+neaktivni) %>%
  left_join(drzava, by = "leto")
row.names(podatki) <- podatki$drzava
podatki$drzava <- NULL

# Število skupin
n <- 5
skupine <- hclust(dist(scale(podatki))) %>% cutree(n)
