
# Modélisation linéaire

library(lubridate)
library(tidyverse)
library(data.table)

library(caret)


# Identification de l'ensemble des bases finales
liste_fichiers <- list.files("Data")
sel_fichiers <- grepl("db_fin_", liste_fichiers)
liste_bases <- liste_fichiers[sel_fichiers]

#liste_region <- c("Antwerp","Belgium","Brussels","East.Flanders","Flanders","Flemish.Brabant","Hainaut","Liège","Limburg","Luxembourg","Namur","Wallonia","Walloon.Brabant","West.Flanders")

for (r in 1:length(liste_bases)) {
#for (r in 1:2) {

# Import base région

region <- unlist(strsplit(liste_bases[r], "_"))[3]
region <- str_replace(region, ".csv", "")

donnees <- read.csv2(paste0("Data/", liste_bases[r]), stringsAsFactors = FALSE)
donnees$dtime_utc <- as.POSIXct(donnees$dtime_utc, format ="%Y-%m-%d %H:%M", tz = "UTC")

donnees$hour <- as.factor(hour(donnees$dtime_utc))
donnees$month <- as.factor(month(donnees$dtime_utc))

# Récupération nom ville
ville <- donnees[1, "ville"]

# suppression colonnes inutiles
donnees <- dplyr::select(donnees, -c(time.x, time.y, icon, summary, ville, time, dtime_utc))

# remplacement NA par 0 sur variables precipitation
donnees$precipAccumulation[is.na(donnees$precipAccumulation)] <- 0
donnees$precipIntensity[is.na(donnees$precipIntensity)] <- 0
donnees$precipProbability[is.na(donnees$precipProbability)] <- 0

# Identification de NA et suppression lignes concernées
# on ne tient pas compte de la variable precipType
# selection via nom et non position
ligne_NA <- rowSums(is.na(donnees[, !grepl("precipType", names(donnees))]))

# suppression des lignes avec au moins 1 NA et suppression de la colonne precipType
donnees <- donnees[!ligne_NA, !grepl("precipType", names(donnees))] 

# suppression des colonnes DA_... et ID_...
don_full <- donnees[,!(names(donnees) %in% c(paste0("DA_LoadFactor_", region), paste0("ID_LoadFactor_", region)))]


#**************************************************

mod.reglin <- train(form = LoadFactor_Antwerp ~ Ieff,
                    data = don_full,
                    method = "lm",
                    metric = "RMSE"
                    )

best.reglin <- mod.reglin$finalModel

save.image("~/ProjetCepeVolt/RMSE_Antwerp.RData")


}