
# Modélisation Ridge, Lasso, Elastic

library(lubridate)
library(tidyverse)
library(data.table)

library(leaps)
library(glmnet)

source("poly_var.R")

# Identification de l'ensemble des bases finales
liste_fichiers <- list.files("Data")
sel_fichiers <- grepl("db_fin_", liste_fichiers)
liste_bases <- liste_fichiers[sel_fichiers]

#liste_region <- c("Antwerp","Belgium","Brussels","East.Flanders","Flanders","Flemish.Brabant","Hainaut","Liège","Limburg","Luxembourg","Namur","Wallonia","Walloon.Brabant","West.Flanders")

# Récupération RMSE pour l'ensemble des modèles linéaires pour les régions
nb_model <- 3 # pour le moment ...
rmse <- matrix(NA, nrow = length(liste_bases), ncol = nb_model)
mape <- matrix(NA, nrow = length(liste_bases), ncol = nb_model)

for (r in 1:length(liste_bases)) {

# Import base région

region <- unlist(strsplit(liste_bases[r], "_"))[3]
region <- str_replace(region, ".csv", "")

donnees <- read.csv2(paste0("Data/", liste_bases[r]), stringsAsFactors = FALSE)
donnees$dtime_utc <- as.POSIXct(donnees$dtime_utc, format ="%Y-%m-%d %H:%M", tz = "UTC")

# Récupération nom ville
ville <- donnees[1, "ville"]

# Selection des heures de "jour" pour éliminer une partie des "0" sur le Load
# arbitrairement : entre 6 et 19 ? A paramétrer ?
sunset <- 6
sunrise <- 19

donnees <- donnees[with(donnees, hour(donnees$dtime_utc) >= sunset & hour(donnees$dtime_utc) <= sunrise), ]

# suppression colonnes inutiles
donnees <- select(donnees, -c(time.x, time.y, icon, summary, ville, time, dtime_utc))

# remplacement NA par 0 sur variables precipitation
donnees$precipAccumulation[is.na(donnees$precipAccumulation)] <- 0
donnees$precipIntensity[is.na(donnees$precipIntensity)] <- 0
donnees$precipProbability[is.na(donnees$precipProbability)] <- 0

## distinction pluie / neige
#donnees$rain <- recode(donnees$precipType, rain = 1, .default = 0, .missing = 0)
#donnees$snow <- recode(donnees$precipType, snow = 1, .default = 0, .missing = 0)
#
## belgique$rainAccumulation <- belgique$precipAccumulation * belgique$rain # vide
#donnees$rainIntensity <- donnees$precipIntensity * donnees$rain
#donnees$rainProbability <- donnees$precipProbability * donnees$rain
#
#donnees$snowAccumulation <- donnees$precipAccumulation * donnees$snow
#donnees$snowIntensity <- donnees$precipIntensity * donnees$snow
#donnees$snowProbability <- donnees$precipProbability * donnees$snow

# a priori, pas d'effets ... à voir si on fait ou pas

# Identification de NA et suppression lignes concernées
# on ne tient pas compte de la variable precipType
# selection via nom et non position
ligne_NA <- rowSums(is.na(donnees[, !grepl("precipType", names(donnees))]))

# suppression des lignes avec au moins 1 NA et suppression de la colonne precipType
donnees <- donnees[!ligne_NA, !grepl("precipType", names(donnees))] 

# suppression des colonnes DA_... et ID_...
don_full <- donnees[,!(names(donnees) %in% c(paste0("DA_LoadFactor_", region), paste0("ID_LoadFactor_", region)))]

# ***********************************************
# Analyse données
# Correlation entre variables

#corr <- cor(don_full, use = "pairwise.complete.obs")
#corrplot::corrplot(corr, type = "upper", method = c("ellipse"))

# corrélation temperature et apparentTemperature, dewPoint --> on garde temperature
# corrélation Ieff et proj --> on garde ?
exclus_corr <- c("apparentTemperature", "dewPoint", "proj")

don_corr <- don_full[, !(names(don_full) %in% exclus_corr)] # suppression : DA, ID, precip, apparentTemp, dewPoint

# sélection variables "manuelle"
sel1 <- c("temperature", "humidity", "cloudCover", "visibility", "Ieff", "proj")


# données centrées/réduites
don_full_sc <- as.data.frame(scale(don_full))
don_corr_sc <- as.data.frame(scale(don_corr))

#**************************************************
# CROSS VALIDATION
#
nrFolds <- 2

# creation vecteur pour affecter chaque ligne à un echantillon
folds <- rep_len(1:nrFolds, nrow(don_full))
folds <- sample(folds, length(folds))

# MODELE RIDGE

data <- don_full_sc # a paramétrer

pred_ridge1 <- rep(NA, length(folds))

for(k in 1:nrFolds) {
  # découpage des données
  fold <- which(folds == k)
  data.train <- data[-fold, ]
  data.test <- data[fold, ]
  
  # modélisation
  ridge1 <- cv.glmnet(as.matrix(data.train[, -1]),
                     as.matrix(data.train[, 1]),
                     alpha = 0,
                     lambda = seq(0, 30, length = 100))
  
#  plot(ridge1)
#  matplot(ridge1$lambda,t(ridge1$glmnet.fit$beta),type="l")
  
  pred_ridge1[fold] <- predict(ridge1, newx = as.matrix(data.test[, -1]))
  
}

# RMSE sur ensemble pred après CV
rmse[r, 1] <- sqrt(mean((pred_ridge1 - data[, 1])^2))
# RMSE sur ensemble pred après CV - si non sc
ape <- sum(abs((pred_ridge1 - data[, 1]) / data[, 1]))
ape <- replace(ape, is.nan(ape)|is.infinite(ape), 0)
mape[r, 1] <- 100*ape/dim(data)[1]


# MODELE REDUIT

data <- don_corr_sc # a paramétrer

pred_ridge2 <- rep(NA, length(folds))

for(k in 1:nrFolds) {
  # découpage des données
  fold <- which(folds == k)
  data.train <- data[-fold, ]
  data.test <- data[fold, ]
  
  # modélisation
  ridge2 <- cv.glmnet(as.matrix(data.train[, -1]),
                      as.matrix(data.train[, 1]),
                      alpha = 0,
                      lambda = seq(0, 30, length = 100))
  
#  plot(ridge2)
#  matplot(ridge2$lambda,t(ridge2$glmnet.fit$beta),type="l")
  
  pred_ridge2[fold] <- predict(ridge2, newx = as.matrix(data.test[, -1]))
  
}

# RMSE sur ensemble pred après CV
rmse[r, 2] <- sqrt(mean((pred_ridge2 - data[, 1])^2))
# RMSE sur ensemble pred après CV - si non sc
ape <- sum(abs((pred_ridge2 - data[, 1]) / data[, 1]))
ape <- replace(ape, is.nan(ape)|is.infinite(ape), 0)
mape[r, 2] <- 100*ape/dim(data)[1]


# MODELE AVEC POLY 3

data <- don_full_sc[, (names(don_full_sc) %in% c(paste0("LoadFactor_", region), sel1))] # a paramétrer

pred_ridge3 <- rep(NA, length(folds))

## écriture formule
#eq <- paste0("poly(", sel1[1], ", 3)")
#for (i in 2:length(sel1)) {
#  eq <- paste0(eq , " + poly(", sel1[i], ", 3)")
#}
#
#eq_model <- as.formula(paste0(paste0("LoadFactor_", region), " ~ ", eq))
# equation "figée" dans le choix des variables
#eq_model <- as.formula(paste0("LoadFactor_", region, " ~ temperature + poly(humidity, 3) + poly(cloudCover, 3) + poly(visibility, 3) + poly(Ieff, 3) + poly(proj, 3)"))

data <- poly_var(data, "humidity", 3)
data <- poly_var(data, "cloudCover", 3)
data <- poly_var(data, "visibility", 3)
data <- poly_var(data, "Ieff", 3)
data <- poly_var(data, "proj", 3)

for(k in 1:nrFolds) {
  # découpage des données
  fold <- which(folds == k)
  data.train <- data[-fold, ]
  data.test <- data[fold, ]
  
  # modélisation
  ridge3 <- cv.glmnet(as.matrix(data.train[, -1]),
                      as.matrix(data.train[, 1]),
                      alpha = 0,
                      lambda = seq(0, 30, length = 100))
  
#  plot(ridge3)
#  matplot(ridge3$lambda,t(ridge3$glmnet.fit$beta),type="l")
  
  pred_ridge3[fold] <- predict(ridge3, newx = as.matrix(data.test[, -1]))

}

# RMSE sur ensemble pred après CV
rmse[r, 3] <- sqrt(mean((pred_ridge3 - data[, 1])^2))
# RMSE sur ensemble pred après CV - si non sc
ape <- sum(abs((pred_ridge3 - data[, 1]) / data[, 1]))
ape <- replace(ape, is.nan(ape)|is.infinite(ape), 0)
mape[r, 3] <- 100*ape/dim(data)[1]

}

matplot(rmse, type="l", lty=1)
legend("topright", lty=1, col=1:3, c("Linéaire complet","Linéaire réduit","Linéaire réduit avec poly 3"))

matplot(mape, type="l", lty=1)
legend("topright", lty=1, col=1:3, c("Linéaire complet","Linéaire réduit","Linéaire réduit avec poly 3"))

