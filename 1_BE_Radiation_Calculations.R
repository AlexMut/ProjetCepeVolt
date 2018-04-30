# PROXY pour les radiations solaires

library(data.table)
library(insol)
library(lubridate)

# Fonctions

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

# Main program

cities <- fread("Data/VillesBel.csv", dec = ",")
names(cities) <- c("lon","lat","city")

prod_cs <- solar_clearsky_prod('01/11/2012', '30/04/2018', cities)

cities_Ieff <- dcast(prod_cs[, c('time', 'city', 'Ieff')], time ~ city, value.var='Ieff')
cities_aoi_proj <- dcast(prod_cs[, c('time', 'city', 'aoi_proj')], time ~ city, value.var='aoi_proj')
write.csv2(cities_Ieff, "Data/Ieff.csv", row.names= FALSE)
write.csv2(cities_aoi_proj, "Data/aoi_proj.csv", row.names= FALSE)


with(prod_cs, plot(time, aoi_proj, 'l'))
with(prod_cs, plot(time, Ieff, 'l'))
