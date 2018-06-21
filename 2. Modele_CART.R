
# Modélisation linéaire

library(lubridate)
library(tidyverse)
library(data.table)

library(caret)
library(rpart.plot) 

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
# CART
#

  objControl <- trainControl(method = "cv",
                             number = 10,
                             allowParallel = TRUE)
  
  gridsearch <- expand.grid(cp = seq(0, 0.1, 0.01))
  
  gridsearch2 <- expand.grid(maxdepth = seq(1, 10, 1))
  
  gridsearch3 <- expand.grid(maxdepth = seq(1, 20, 1))
  
  # Paramètrage CP
  # Modèles full variables sans interaction
  cartcp1.rmse <- train(LoadFactor ~ .,
                      data = don_full[, -c(DA, ID)],
                      method = "rpart",
                      tuneGrid = gridsearch, 
                      trControl = objControl,
                      metric = "RMSE")
  
  saveRDS(cartcp1.rmse, file = paste0("cartcp1.rmse_", r, ".rds"))
  
  cartcp1.mae <- train(LoadFactor ~ .,
                     data = don_full[, -c(DA, ID)],
                     method = "rpart",
                     tuneGrid = gridsearch, 
                     trControl = objControl,
                     metric = "MAE")
  
  saveRDS(cartcp1.mae, file = paste0("cartcp1.mae_", r, ".rds"))
  
  # Modèles selection variables avec interaction
  cartcp2.rmse <- train(LoadFactor ~ (Ieff + cloudCover + proj + humidity):hour,
                          data = don_full[, -c(DA, ID)],
                    method = "rpart",
                    tuneGrid = gridsearch, 
                    trControl = objControl,
                    metric = "RMSE")
  
  saveRDS(cartcp2.rmse, file = paste0("cartcp2.rmse_", r, ".rds"))
  
  cartcp2.mae <- train(LoadFactor ~ (Ieff + cloudCover + proj + humidity):hour,
                    data = don_full[, -c(2, 3)],
                    method = "rpart",
                    tuneGrid = gridsearch, 
                    trControl = objControl,
                    metric = "MAE")
  
  saveRDS(cartcp2.mae, file = paste0("cartcp2.mae_", r, ".rds"))
  
  # Paramètrage MAXDEPTH
  # Modèles full variables sans interaction
  cartmd1.rmse <- train(LoadFactor ~ .,
                        data = don_full[, -c(DA, ID)],
                        method = "rpart2",
                        tuneGrid = gridsearch2, 
                        trControl = objControl,
                        metric = "RMSE")
  
  saveRDS(cartmd1.rmse, file = paste0("cartmd1.rmse_", r, ".rds"))
  
  cartmd1.mae <- train(LoadFactor ~ .,
                       data = don_full[, -c(DA, ID)],
                       method = "rpart2",
                       tuneGrid = gridsearch2, 
                       trControl = objControl,
                       metric = "MAE")
  
  saveRDS(cartmd1.mae, file = paste0("cartmd1.mae_", r, ".rds"))
  
  # Modèles selection variables avec interaction
  cartmd2.rmse <- train(LoadFactor ~ (Ieff + cloudCover + proj + humidity):hour,
                      data = don_full[, -c(DA, ID)],
                      method = "rpart2",
                      tuneGrid = gridsearch3, 
                      trControl = objControl,
                      metric = "RMSE")
  
  saveRDS(cartmd2.rmse, file = paste0("cartmd2.rmse_", r, ".rds"))
  
  cartmd2.mae <- train(LoadFactor ~ (Ieff + cloudCover + proj + humidity):hour,
                     data = don_full[, -c(2, 3)],
                     method = "rpart2",
                     tuneGrid = gridsearch3, 
                     trControl = objControl,
                     metric = "MAE")
  
  saveRDS(cartmd2.mae, file = paste0("cartmd2.mae_", r, ".rds"))
  
}
  
stopCluster(cluster)

# Pour récupérer les RMSE et MAE
#  mod.cart.rmse$results$RMSE[which(mod.cart.rmse$results$cp == mod.cart.rmse$bestTune)]
#  mod.cart.mae$results$MAE[which(mod.cart.mae$results$cp == mod.cart.mae$bestTune)]

