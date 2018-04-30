#Fusion des fichiers météorologiques par ville

library(plyr) # pour fonction rbind.fill 


## Import des fichiers 
# Récupération de la listes de l'ensemble des fichiers du répertoire Data 
liste_fichiers <- list.files("Data/Météo") 


# Sélection des fichiers avec "_" 
liste_fichiers <- liste_fichiers[grep("_", liste_fichiers)] 


# Import de l'ensemble des fichiers retenu - attention au séparateur et au symbole des décimales 
All <- lapply(liste_fichiers, function(i){ 
  read.table(paste0("Data/Météo/",i), header = TRUE, sep = ";", dec = ",") 
}) 


# Ajout du nom de la ville dans les fichiers et passage à l'heure UTC 
for (i in 1:length(All)) { 
  All[[i]]$ville <- strsplit(liste_fichiers[i], "_")[[1]][1] 
  All[[i]]$time <- as.POSIXct(All[[i]]$time, tz="Europe/Brussels")
  attr(All[[i]]$time, "tzone") <- "UTC"
  print(All[[i]][duplicated(All[[i]]$time),c("ville","time")])
  All[[i]]$time[duplicated(All[[i]]$time)] <- All[[i]]$time[duplicated(All[[i]]$time)]+60*60
} 


# Fusion des fichiers 
df <- do.call("rbind.fill", All) 

## Ecriture fichier fusionné 
# Récupération nom des villes pour nom fichier propre à chacun 
liste_ville <- unique(lapply(liste_fichiers, function(i) {strsplit(i, "_")[[1]][1]})) 


# Cumul nom villes 
nom_fichier <- do.call("paste0", liste_ville) 


# Ecriture 
write.csv2(df[,-1], paste0("Data/Météo/", nom_fichier, ".csv"), row.names = FALSE) 


