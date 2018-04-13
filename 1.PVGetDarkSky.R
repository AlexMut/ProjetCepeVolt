
# Récupération données météo Dark Sky

library(darksky)

source('darksky_key.R')

# fonction pour récupérer les données par heure : 
# - ville : donner nom et coorespondance coord via base. Attention 
# - période : donner année, définition période sur 2 ans pour requête

get_meteo <- function(ville, periode) {
  
  #debut <- as.Date(paste(periode-1, "-01-01", sep = ""))
  debut <- as.Date(paste(periode, "-11-01", sep = "")) # pour tester sur 2 mois
  fin <- as.Date(paste(as.character(periode), "-12-31", sep = ""))
  histo <- seq(debut, fin, by="day")
  histo <- as.list(paste(histo, "T00:00:00-0400", sep = ""))
  
  coord_ville <- read.csv2("Data/VillesBel.csv")
  lat <- coord_ville$Lat[coord_ville$Ville == ville]
  long <- coord_ville$Long[coord_ville$Ville == ville]
  
  tmp <- lapply(histo, 
                function(date) { 
                  get_forecast_for(lat, long, date, units = "ca", language = "fr")$hourly 
                }) # sortie en data frame
  
  do.call("rbind.fill", tmp) # appel recursif de rbind pour fusionner les éléments de liste tmp
  
}

tmp <- get_meteo("Zaventem", 2017)

write.csv2(tmp, paste0(paste("Data/", ville, periode, sep = "_"), ".csv"))

