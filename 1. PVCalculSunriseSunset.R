
# Calcul de sunrise et sunset par ville et par jour

library(suncalc)
library(lubridate)

# import données
data <- read.csv2("Data/ElsenbornFlorennesGentKleine Brogel.csv", stringsAsFactors = FALSE)

coord_villes <- read.csv2("Data/VillesLatLong.csv", stringsAsFactors = FALSE, col.names = c("cp", "ville", "lon", "lat"))
# changement des noms pour la fusion (Ville vs ville) et pour la formule getSunLight (lat, lon)

# écriture différente pour Kleine Brogel entre les 2 fichiers
# " " dans le fichier météo
# "-" dans le fichier coord
# ATTENTION : voir si autres cas de figures (accents, ...)
# identification et remplacement
posKB <- grep("Kleine[-]Brogel", coord_villes$ville)
coord_villes$ville[posKB] <- "Kleine Brogel"

# Récupération coord GPS villes
data2 <- merge(data, coord_villes, by = "ville")
data2$date <- as.Date(data2$time)

# Pb : pas toujours les mêmes heurs/jours.
# Si sélection unicité via une heure, on peut avoir des manques !!

# Récupération de la liste des jours uniques ville x date
jour_unique <- unique(data2[, c("ville", "date", "lat", "lon")])

#write.csv2(jour, "Data/jour.csv", row.names = FALSE)

#Calcul sunrise / sunset sur table jour_unique pour limiter le nombre de calcul
jour_unique$sunrise <- getSunlightTimes(data = jour_unique)$sunrise
jour_unique$sunset <- getSunlightTimes(data = jour_unique)$sunset

#Intégration de sunrise / sunset avec météo 
data3 <- merge(data2, jour_unique, by = c("ville", "date"))

# Ecriture
write.csv2(data3, "Data/MeteoSol.csv", row.names = FALSE)


