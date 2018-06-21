
# Modélisation linéaire

library(lubridate)
library(tidyverse)
library(data.table)

library(caret)

library(parallel)
library(doParallel)

cluster <- makeCluster(detectCores()-1)
registerDoParallel(cluster)


# Identification de l'ensemble des bases finales
liste_fichiers <- list.files("Data")
sel_fichiers <- grepl("db_fin_", liste_fichiers)
liste_bases <- liste_fichiers[sel_fichiers]

#liste_region <- c("Antwerp","Belgium","Brussels","East.Flanders","Flanders","Flemish.Brabant","Hainaut","Liège","Limburg","Luxembourg","Namur","Wallonia","Walloon.Brabant","West.Flanders")

for (r in 1:length(liste_bases)) {
  
  # Import base région
  
  region <- unlist(strsplit(liste_bases[r], "_"))[3]
  region <- str_replace(region, ".csv", "")
  
  donnees <- read.csv2(paste0("Data/", liste_bases[r]), stringsAsFactors = FALSE)
  donnees$dtime_utc <- as.POSIXct(donnees$dtime_utc, format ="%Y-%m-%d %H:%M", tz = "UTC")
  
  donnees$hour <- as.factor(hour(donnees$dtime_utc))
  #donnees$month <- as.factor(month(donnees$dtime_utc))
  
  # Récupération nom ville
  ville <- donnees[1, "ville"]
  
  # suppression colonnes inutiles
  donnees <- dplyr::select(donnees, -c(ville, dtime_utc))
  
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
  don_full <- donnees
  # position variables DA et ID pour exclusion
  DA <- grep("DA_LoadFactor", names(don_full))
  ID <- grep("ID_LoadFactor", names(don_full))
  
  
#**************************************************
# REGRESSION
#

  objControl <- trainControl(method = "cv",
                             number = 10,
                             allowParallel = TRUE)
  
  gridsearch = expand.grid(k = c(3,5,10))


  # Modèles full variables sans interaction
  knn1.rmse <- train(LoadFactor ~ .,
                     data = don_full[, -c(DA, ID)],
                     method = "knn",
                     trControl = objControl,
                     tuneGrid = gridsearch,
                     preProc = c("center", "scale"),
                     metric = "RMSE")
  
  saveRDS(knn1.rmse, file = paste0("RDS/knn1.rmse_", r, ".rds"))
  
  knn1.mae <- train(LoadFactor ~ .,
                     data = don_full[, -c(DA, ID)],
                     method = "knn",
                     trControl = objControl,
                     tuneGrid = gridsearch,
                     preProc = c("center", "scale"),
                     metric = "MAE")
  
  saveRDS(knn1.mae, file = paste0("RDS/knn1.mae_", r, ".rds"))
  
  # Modèles selection variables avec interaction 2 à 2
  knn2.rmse <- train(LoadFactor ~ (Ieff + cloudCover + proj + humidity):hour,
                     data = don_full[, -c(DA, ID)],
                     method = "knn",
                     trControl = objControl,
                     preProc = c("center", "scale"),
                     tuneGrid = gridsearch,
                     metric = "RMSE")
  
  saveRDS(knn2.rmse, file = paste0("RDS/knn2.rmse_", r, ".rds"))
  
  knn2.mae <- train(LoadFactor ~ (Ieff + cloudCover + proj + humidity):hour,
                     data = don_full[, -c(DA, ID)],
                     method = "knn",
                     trControl = objControl,
                     preProc = c("center", "scale"),
                     tuneGrid = gridsearch,
                     metric = "MAE")
  
  saveRDS(knn2.mae, file = paste0("RDS/knn2.mae_", r, ".rds"))
  
  # Modèles selection variables
  knn3.rmse <- train(LoadFactor ~ Ieff + cloudCover + proj + humidity + hour,
                     data = don_full[, -c(DA, ID)],
                     method = "knn",
                     trControl = objControl,
                     preProc = c("center", "scale"),
                     tuneGrid = gridsearch,
                     metric = "RMSE")
  
  saveRDS(knn3.rmse, file = paste0("RDS/knn3.rmse_", r, ".rds"))
  
  knn3.mae <- train(LoadFactor ~ Ieff + cloudCover + proj + humidity + hour,
                    data = don_full[, -c(DA, ID)],
                    method = "knn",
                    trControl = objControl,
                    preProc = c("center", "scale"),
                    tuneGrid = gridsearch,
                    metric = "MAE")
  
  saveRDS(knn2.mae, file = paste0("RDS/knn3.mae_", r, ".rds"))

  
}
  
stopCluster(cluster)



