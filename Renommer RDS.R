
# Récupération des metrics modèle .RDS --> fonction

library(tidyverse)
library(stringr)

# Modèles stockés sous format .RDS
liste_fichiers <- as.list(list.files("RDS"))
liste_region <- c("Antwerp","Belgium","Brussels","East.Flanders","Flanders","Flemish.Brabant","Hainaut","Liège","Limburg","Luxembourg","Namur","Wallonia","Walloon.Brabant","West.Flanders")

renomme <- function(liste_fichiers) {
  
  lapply(liste_fichiers, function(fic){

  mod <- readRDS(paste("RDS/", fic, sep = ""))
    
  tmp1 <- str_split(fic, "_")[[1]][1]
  tmp2 <- str_split(fic, "_")[[1]][2]
  tmp3 <- str_split(tmp2, "\\.")[[1]][1]
  tmp3 <- as.numeric(tmp3)
    
  new_nom <- paste0(paste(tmp1, liste_region[tmp3], sep = "_"), ".rds")
    
  saveRDS(mod, file = paste("RDS/", new_nom, sep = ""))
    
  })
}

renomme(liste_fichiers)
