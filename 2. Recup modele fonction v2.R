
# Récupération des metrics modèle .RDS --> fonction

library(tidyverse)
library(stringr)

# Modèles stockés sous format .RDS
liste_fichiers <- as.list(list.files("RDS"))

recup_modele2 <- function(type_model, liste_fichiers) {
  
  liste_modele <- liste_fichiers[grepl(paste0("^", type_model), liste_fichiers)]
  
  All <- lapply(liste_modele, function(mod){
    res.mod <- readRDS(paste("RDS/", mod, sep = ""))
    
    if (type_model %in% c("reglin", "cart", "knn")) {
      rmse <- res.mod$results$RMSE[which(res.mod$results[, 1] == as.numeric(res.mod$bestTune))]
      mae <- res.mod$results$MAE[which(res.mod$results[, 1] == as.numeric(res.mod$bestTune))]
    }
    
    if (type_model %in% c("ridge", "lasso")) {
      rmse <- res.mod$results$RMSE[which(res.mod$results[, 2] == as.numeric(res.mod$bestTune[2]))]
      mae <- res.mod$results$MAE[which(res.mod$results[, 2] == as.numeric(res.mod$bestTune[2]))]
    }
    
    as.data.frame(list(str_remove(mod, ".rds"), rmse, mae), col.names = c("model", "rmse", "mae"), stringsAsFactors = FALSE)
    
  })
  
  resume <- do.call(plyr::rbind.fill, All)
  
  resume$nom_model <- str_split_fixed(resume$model, "_", 2)[, 1]
  resume$region <- str_split_fixed(resume$model, "_", 2)[, 2]
  
  resume$metric <- str_split_fixed(resume$nom_model, "\\.", 2)[, 2]
  resume$nom_model <- str_split_fixed(resume$nom_model, "\\.", 2)[, 1]
  
  resume$value <- ifelse(resume$metric == "rmse", resume$rmse, resume$mae)
  
  ### version "long"
  resume <- select(resume, nom_model, region, metric, value)
  
  ### version "wide"
  resume_w <- spread(resume, key = metric, value = value)
  
}

#reglin <- recup_modele2("reglin", liste_fichiers)
#lasso <- recup_modele2("lasso", liste_fichiers)
#ridge <- recup_modele2("ridge", liste_fichiers)
##cart <- recup_modele2("cart", liste_fichiers)
#knn <- recup_modele2("knn", liste_fichiers)

#reglin %>% 
#  ggplot(aes(x = nom_model, y = value, fill = metric)) +
#  geom_bar(stat="identity", position = position_dodge()) +
#  geom_text(aes(label = round(value, 1)), vjust = -.5, #color="white",
#            position = position_dodge(0.9), size = 3.5) +
#  facet_wrap(~region)

#reglin %>% 
#  ggplot(aes(mae, rmse, shape = as.factor(nom_model), col = as.factor(region))) +
#  geom_point()

