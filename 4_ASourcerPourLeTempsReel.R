# sources pour shiny en temps réel

library(darksky)
library(plyr)

library(data.table)
library(insol)
library(lubridate)

library(RCurl)
library(jsonlite)

#Sys.setenv(DARKSKY_API_KEY = "49c41142c99f93e0323c5cd61ba4d7b7") #emilie
Sys.setenv(DARKSKY_API_KEY = "a051df2cff6ac67ea63b999dee0a918e")

# Sys.setenv(HTTP_PROXY="10.172.209.33:80")
# Sys.setenv(HTTPS_PROXY="10.172.209.33:80")
opts <- list()

#---------
#FONCTIONS
#---------

# Récupération données météo Dark Sky 
#------------------------------------
get_meteo_now <- function(coord_ville, ville) {
  lat <- coord_ville$Lat[coord_ville$Ville == ville]
  long <- coord_ville$Long[coord_ville$Ville == ville]
  tmp <- get_current_forecast(lat, long, units = "ca", language = "fr", expand="hourly")$hourly #df
  #attention heure locale !
  tmp$dtime_utc <- as.POSIXct(tmp$time, tz="Europe/Brussels")
  attr(tmp$dtime_utc, "tzone") <- "UTC"
  return(tmp)
}

# Calcul des radiations solaires
#------------------------------
solar_position <- function(data){
  # Sun position (require insol package)
  data[, c('s_azim', 's_zeni') := {
    jd <- JD(time + (time[2] - time[1])/2) # Julian date at the center of the time interval
    tmp <- sunpos(sunvector(jd, lat, lon, +0)) 
    .(tmp[, 'azimuth'], tmp[,  'zenith'])}]
  return(data)
}

solar_optimal_aoi_proj <- function(lat, lon, s_azim, s_zeni){
  
  aoi_proj <- function(p, s_azim, s_zeni, p_azim){
    # cos O = cos(a)*sin(p_tilt)*cos(c) + sin(a)*cos(p_tilt)
    #     with  a = 90 - s_zeni
    #           c = p_azim - s_azim
    cos_aoi = cos(radians(90-s_zeni))*sin(radians(p[1]))*cos(radians(p_azim - s_azim)) + 
      sin(radians(90-s_zeni))*cos(radians(p[1]))
    
    return(pmax(0, cos_aoi))
  }
  
  
  p_azim = 180*(unique(lat) >= 0)
  cos_aoi = aoi_proj(45, s_azim, s_zeni, p_azim)
  return(data.table(tilt = 45,
                    cos_aoi = cos_aoi))
}

solar_simple_effective_irrad <- function(data, SC = 1367, E0 = 1000){
  
  ## 0. Initialization ####
  yd <- with(data, 
             yday(time) + hour(time)/24 + 
               minute(time)/24/60 + second(time)/24/60/60)
  da <- 2*pi*(yd-1)/365
  cosz <- cos(radians(data$s_zeni)) * (data$s_zeni <= 90)
  
  ## 1. Extraterrestrial radiation ####
  dni_extra <- SC*(1.00011 + 0.034221 * cos(da) + 0.00128 * sin(da) +
                     0.000719 * cos(2 * da) + 7.7e-05 * sin(2 * da))
  
  ## 2. Air Mass & Direct Normal Irradiance ####
  AM <- (1.002432*cosz^2 + 0.148386*cosz + 0.0096467)/
    (cosz^3 + 0.149864*cosz^2 + 0.0102963*cosz + 0.000303978)
  dni = dni_extra * pmax(0, 0.7^AM^0.678)
  
  ## X. Beam component & effective irradiance ####
  beam <- pmax(0, dni * data$aoi_proj)
  Ieff = beam / E0
  
  return(data.table(dni_extra = dni_extra,
                    dni = dni,
                    Ieff = Ieff))
}

solar_clearsky_prod <- function(time_start='01/01/2016', time_end='31/12/2016', 
                                locations){
  
  # Time range
  time_start <- as.POSIXct(time_start, format='%d/%m/%Y', tz='UTC')
  time_end <- as.POSIXct(time_end, format='%d/%m/%Y', tz='UTC') + hours(x=23)
  time_seq <- seq(time_start, time_end, by='hour')
  
  # Locations
  dt = locations[rep(seq_len(nrow(locations)), each=length(time_seq))]
  dt[, time := time_seq, by=.(city)]
  
  # AOI projection
  dt <- solar_position(dt)
  dt[, c('tilt', 'aoi_proj') := solar_optimal_aoi_proj(lat, lon, s_azim, s_zeni), by=city]
  dt[, c('dni_extra', 'dni', 'Ieff') := solar_simple_effective_irrad(dt)]
  
  # aoi_proj = angle of incidence proj. (cosine of the angle between the solar beam and the PV panel's normal - tilt =45°)
  # dni_extra = extraterrestrial direct normal irradiance (top of the atmoshpere radiation)
  # Ieff = effective irradiance (Ieff = max(0, dni * aoi_proj) with the dni = direct normal irradiance on the Earth' surface)
  
  return(dt)
}

# Récupérations des données PV prévues d'Elia
#------------------------------
pv_data <- function(){
  #prévions
  name.url <- paste("http://publications.elia.be/Publications/publications/solarforecasting.v3.svc/GetChartDataForZone?dateFrom=",Sys.Date(), "&dateTo=", Sys.Date()+2, "&sourceId=", 1, sep="")
  json <- getURL(name.url,.opts=opts)
  data <- fromJSON(json)
  donnees <- cbind(data$SolarForecastingChartDataForZoneItems$StartsOn[1],
                   data$SolarForecastingChartDataForZoneItems[c("Forecast", "ForecastUpdated")])
  names(donnees)=c("Dates",paste(c("Forecast", "ForecastUpdated"),regions[1],sep="_"))
  donnees$Dates <- as.POSIXct(as.numeric(substr(donnees$Dates,7,19))/1000, origin = "1970-01-01",tz = "UTC")
  for (i in 2:14)
  {
    name.url <- paste("http://publications.elia.be/Publications/publications/solarforecasting.v3.svc/GetChartDataForZone?dateFrom=", Sys.Date(), "&dateTo=",  Sys.Date()+2, "&sourceId=", i, sep="")
    json <- getURL(name.url,.opts=opts)
    data <- fromJSON(json)
    tmp  <- data$SolarForecastingChartDataForZoneItems[c("Forecast", "ForecastUpdated")]
    names(tmp) <- paste(c("Forecast", "ForecastUpdated"),regions[i],sep="_")
    donnees <- cbind(donnees,tmp)
  }
  donnees<- donnees[which(duplicated(donnees)=="FALSE"),]
  donnees[which(donnees == (-50)),] <- NA
  
  #capacités
  nurl <- "http://publications.elia.be/Publications/publications/solarforecasting.v3.svc/GetCapacities?dateFrom=2018-03-01&dateTo=2018-03-31&sourceId="
  for (r in 1:length(regions))
  {
    name.url <- paste(nurl, r, sep="")
    json  <- getURL(name.url,.opts=opts)
    data  <- fromJSON(json)
    capa <- data$Power
    donnees[paste("DA_LoadFactor",regions[r], sep="_")] <- donnees[paste("Forecast", regions[r], sep="_")]/capa*100
    donnees[paste("ID_LoadFactor",regions[r], sep="_")] <- donnees[paste("ForecastUpdated", regions[r], sep="_")]/capa*100
    
  }
  donnees$dtime_utc   <- as.POSIXct(donnees$Dates, format ="%Y-%m-%d %H:%M", tz = "UTC")
  return(donnees)
}

#--------------------
# Programme principal
#--------------------

#ce qui peut etre lancé avant
#---------------------------
regions <- c("Belgium", "Flanders","Wallonia","Brussels","Antwerp","Hainaut","Limburg",
             "Liège","Luxembourg","Namur","East-Flanders","Flemish-Brabant",
             "Walloon-Brabant","West-Flanders")

# données PV prévues pour toutes les zones
pv <- pv_data()
pv$heure_utc <- floor_date(as.POSIXct(pv$dtime_utc, format = "%Y-%m-%d %H:%M:%S"), unit = "hour")

#calcul sur toutes les villes des radiations solaires
coord_ville <- fread("Data/VillesBel.csv", dec = ",")
cities <- coord_ville; names(cities) <- c("lon","lat","city")
prod_cs <- solar_clearsky_prod(Sys.Date(),Sys.Date()+2, cities)
Ieff <- as.data.frame(dcast(prod_cs[, c('time', 'city', 'Ieff')], time ~ city, value.var='Ieff'))
proj <- as.data.frame(dcast(prod_cs[, c('time', 'city', 'aoi_proj')], time ~ city, value.var='aoi_proj'))
Ieff$dtime_utc <- as.character(as.POSIXct(Ieff$time, format = "%Y-%m-%d %H:%M:%S"))
proj$dtime_utc <- as.character(as.POSIXct(proj$time, format = "%Y-%m-%d %H:%M:%S"))

villes <- read.csv2("Data/VillesBelReg.csv", stringsAsFactors = FALSE)
