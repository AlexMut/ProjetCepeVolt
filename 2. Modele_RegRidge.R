
# Modélisation linéaire

library(lubridate)
library(tidyverse)
library(data.table)

library(caret)

library(glmnet)
library(Matrix)

library(parallel)
library(doParallel)

cluster <- makeCluster(5)
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
  
  # REGRESSION RIDGE
  
  gridsearchR <- expand.grid(alpha = 1,
                            lambda = seq(0, 30, length = 100))
  
  # Modèles full variables sans interaction
  ridge1.rmse <- train(LoadFactor ~ .,
                       data = don_full[, -c(DA, ID)],
                       method = "glmnet",
                       trControl = objControl,
                       tuneGrid = gridsearchR,
                       metric = "RMSE")
  
  saveRDS(ridge1.rmse, file = paste0("RDS/ridge1.rmse_", r, ".rds"))
  
  ridge1.mae <- train(LoadFactor ~ .,
                      data = don_full[, -c(DA, ID)],
                      method = "glmnet",
                      trControl = objControl, 
                      tuneGrid = gridsearchR,
                      metric = "MAE")
  
  saveRDS(ridge1.mae, file = paste0("RDS/ridge1.mae_", r, ".rds"))
  
  # Modèles selection variables avec interaction 2 à 2
  ridge2.rmse <- train(LoadFactor ~ (Ieff + cloudCover + proj + humidity):hour,
                       data = don_full[, -c(DA, ID)],
                       method = "glmnet",
                       trControl = objControl,
                       tuneGrid = gridsearchR,
                       metric = "RMSE")
  
  saveRDS(ridge2.rmse, file = paste0("RDS/ridge2.rmse_", r, ".rds"))
  
  ridge2.mae <- train(LoadFactor ~ (Ieff + cloudCover + proj + humidity):hour,
                      data = don_full[, -c(DA, ID)],
                      method = "glmnet",
                      trControl = objControl,
                      tuneGrid = gridsearchR,
                      metric = "MAE")
  
  saveRDS(ridge2.mae, file = paste0("RDS/ridge2.mae_", r, ".rds"))
  
  # Modèles selection variables avec interactions
  ridge3.rmse <- train(LoadFactor ~ Ieff:cloudCover:proj:humidity:hour,
                        data = don_full[, -c(DA, ID)],
                       method = "glmnet",
                       trControl = objControl,
                       tuneGrid = gridsearchR,
                        metric = "RMSE")
  
  saveRDS(ridge3.rmse, file = paste0("RDS/ridge3.rmse_", r, ".rds"))
  
  ridge3.mae <- train(LoadFactor ~ Ieff:cloudCover:proj:humidity:hour,
                       data = don_full[, -c(DA, ID)],
                      method = "glmnet",
                      trControl = objControl,
                      tuneGrid = gridsearchR,
                       metric = "MAE")
  
  saveRDS(ridge3.mae, file = paste0("RDS/ridge3.mae_", r, ".rds"))

  
}
  
stopCluster(cluster)



