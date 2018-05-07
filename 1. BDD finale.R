
# Fusion bases météo + radiations + PV

library(lubridate)

regions <- c("Belgium", "Flanders","Wallonia","Brussels","Antwerp","Hainaut","Limburg",
             "Liège","Luxembourg","Namur","East.Flanders","Flemish.Brabant",
             "Walloon.Brabant","West.Flanders")

#regions <- "Limburg"

for (i in 1:length(regions)) {
  
r <- regions[i]

# PV

tmp <- read.csv2("Data/PV/Table_AUTO2.csv")

sel <- c("dtime_utc", paste(c("LoadFactor", "DA_LoadFactor", "ID_LoadFactor"), r, sep = "_"))

pv <- tmp[which(tmp[, sel[2]] != -50), sel]
pv <- na.omit(pv)

pv$heure_utc <- floor_date(as.POSIXct(pv$dtime_utc, format = "%Y-%m-%d %H:%M:%S"), unit = "hour")

pv_heure <- aggregate(pv[, -c(1,5)], list(dtime_utc = pv$heure_utc), sum)

# Données météo

villes <- read.csv2("Data/VillesBelReg.csv", stringsAsFactors = FALSE)

station <- villes[villes$Région == r, "Ville"]

meteo <- read.csv2("Data/Météo/MeteoFin.csv", stringsAsFactors = FALSE)

# Pb nom pour Saint Hubert, Louvain-la-neuve et Kleine Brogel
# Remplacement de " " et "-" par "." pour homogénéiser les noms
meteo$ville <- gsub(" ", ".", meteo$ville)
meteo$ville <- gsub("-", ".", meteo$ville)

meteo2 <- meteo[which(meteo$ville == station), ]

# radiations

Ieff <- read.csv2("Data/Radiations/Ieff.csv")
Ieff2 <- Ieff[, c("time", station)]
colnames(Ieff2)[2] <- "Ieff"

proj <- read.csv2("Data/Radiations/aoi_proj.csv")
proj2 <- proj[, c("time", station)]
colnames(proj2)[2] <- "proj"


# Fusion

pv_heure$dtime_utc <- as.character(as.POSIXct(pv_heure$dtime_utc, format = "%Y-%m-%d %H:%M:%S"))

meteo2$dtime_utc <- as.character(as.POSIXct(meteo2$time, format = "%Y-%m-%d %H:%M:%S"))

Ieff2$dtime_utc <- as.character(as.POSIXct(Ieff2$time, format = "%Y-%m-%d %H:%M:%S"))

proj2$dtime_utc <- as.character(as.POSIXct(proj2$time, format = "%Y-%m-%d %H:%M:%S"))

db_fin <- merge(pv_heure, meteo2, by = "dtime_utc", all.x = TRUE)

db_fin <- merge(db_fin, Ieff2, by = "dtime_utc", all.x = TRUE)

db_fin <- merge(db_fin, proj2, by = "dtime_utc", all.x = TRUE)

write.csv2(db_fin, paste("Data/db_fin_", r, ".csv", sep = ""), row.names = FALSE)

}
