#-------------------------------------------------
#récupération données PV par région de la Belgique
#-------------------------------------------------

# /!\ très long à touner => A optimiser en enlevant les boucles et en utilisant du datatable ? en supprimant la sauvegarde au milieu du code ?
# mes proxys sont inutiles pour vous :)
#opts <- list(proxy="proxygin.melinda.local", proxyport=8080)
opts <- list()
#dates à modifier, commencez petit :)
histo <- seq(as.Date("2013-01-01"),as.Date("2017-12-31"), by="month")


library('RCurl')
library('jsonlite')

regions <- c("Belgium", "Flanders","Wallonia","Brussels","Antwerp","Hainaut","Limburg",
             "Liège","Luxembourg","Namur","East-Flanders","Flemish-Brabant",
             "Walloon-Brabant","West-Flanders")

# Récupération des volumes
#-------------------------

donnees_fin <- NULL
for (m in 1: (length(histo)-1))
{
  print(histo[m])
  name.url <- paste("http://publications.elia.be/Publications/publications/solarforecasting.v3.svc/GetChartDataForZone?dateFrom=", histo[m], "&dateTo=",  histo[m+1], "&sourceId=", 1, sep="")
  json <- getURL(name.url,.opts=opts)
  data <- fromJSON(json)
  donnees <- cbind(data$SolarForecastingChartDataForZoneItems$StartsOn[1],
                   data$SolarForecastingChartDataForZoneItems[c("Forecast", "ForecastUpdated","LoadFactor", "RealTime")])
  names(donnees)=c("Dates",paste(c("Forecast", "ForecastUpdated", "LoadFactor","RealTime"),regions[1],sep="_"))
  donnees$Dates <- as.POSIXct(as.numeric(substr(donnees$Dates,7,19))/1000, origin = "1970-01-01",tz = "UTC")
  for (i in 2:14)
  {
    name.url <- paste("http://publications.elia.be/Publications/publications/solarforecasting.v3.svc/GetChartDataForZone?dateFrom=", histo[m], "&dateTo=", histo[m+1], "&sourceId=", i, sep="")
    json <- getURL(name.url,.opts=opts)
    data <- fromJSON(json)
    tmp  <- data$SolarForecastingChartDataForZoneItems[c("Forecast", "ForecastUpdated","LoadFactor", "RealTime")]
    names(tmp) <- paste(c("Forecast", "ForecastUpdated", "LoadFactor","RealTime"),regions[i],sep="_")
    donnees <- cbind(donnees,tmp)
  }
  donnees_fin <- rbind(donnees_fin, donnees)
}
donnees_fin <- donnees_fin[which(duplicated(donnees_fin)=="FALSE"),]
write.csv2(donnees_fin,"Data/Table_AUTO.csv", row.names=FALSE)

# récupération des capacités
#-------------------------

capa <- function(nurl, regions, dates)
{
  capa <- NULL
  for (r in 1:length(regions))
  {
    name.url <- paste(nurl, r, sep="")
    json  <- getURL(name.url,.opts=opts)
    data  <- fromJSON(json)
    donnees <- data$Power
    names(donnees)=paste("Power",regions[r],sep="_")
    capa  <- c(capa,donnees)
  }
  parts <- round(capa/capa[1]*100,2)
  names(parts)=paste("Weights",regions,sep="_")
  pie(capa[-(1:3)],labels = paste(regions[-(1:3)],": ",parts[-(1:3)]," %"),col = cm.colors(14), radius = 0.9, main = paste("ELIA Regions weights for PV production",dates, sep=" "),cex=4, cex.main=4)
  return(cbind(capa,parts))
}

op <- par(mfrow = c(1,3))
elia_before <- capa("http://publications.elia.be/Publications/publications/solarforecasting.v3.svc/GetCapacities?dateFrom=2014-01-01&dateTo=2016-01-31&sourceId=", regions, "before 10/09/2015")
elia_2015  <- capa("http://publications.elia.be/Publications/publications/solarforecasting.v3.svc/GetCapacities?dateFrom=2016-01-01&dateTo=2016-01-31&sourceId=", regions, "after 10/09/2015") # change i, 10/09/2015
elia_2018  <- capa("http://publications.elia.be/Publications/publications/solarforecasting.v3.svc/GetCapacities?dateFrom=2018-03-01&dateTo=2018-03-31&sourceId=", regions, "after 21/02/2018") # change 21/02/2018
par(op)



# Calcul des LoadFactor
#----------------------

pv_data <- read.csv2("Data/PV/Table_AUTO.csv")
pv_data$dtime_utc   <- as.POSIXct(pv_data$Dates, format ="%Y-%m-%d %H:%M", tz="Europe/Brussels")
attr(pv_data$dtime_utc, "tzone") <- "UTC"

for (r in 1:length(regions))
{
  capa_r <- ifelse(pv_data$dtime_utc <="2015-09-09 22:00", 
                   elia_before[r,1], 
                   ifelse(pv_data$dtime_utc <="2018-02-20 23:00",
                          elia_2015[r,1],
                          elia_2018[r,1]))
  pv_data[paste("DA_LoadFactor",regions[r], sep="_")] <- pv_data[paste("Forecast", regions[r], sep="_")]/capa_r*100
  pv_data[paste("ID_LoadFactor",regions[r], sep="_")] <- pv_data[paste("ForecastUpdated", regions[r], sep="_")]/capa_r*100
  }
write.csv2(pv_data,"Data/Table_AUTO2.csv", row.names=FALSE) # Fichier de travail avec LoadFactor



# Fusion
#----------------------
# library(readxl)
# library(lubridate)
# weather_data <- read_excel("Data/weather_data.xlsx",skip=0,col_names=TRUE)
# weather_data$dtime_utc <- round_date(weather_data$date, "hour")      #UTC
# weather_data$Year      <- as.numeric(format(weather_data$dtime_utc ,"%Y"))
# weather_data$Month     <- as.numeric(format(weather_data$dtime_utc ,"%m"))
# weather_data$Day       <- as.numeric(format(weather_data$dtime_utc ,"%d"))
# weather_data$Hour      <- as.numeric(format(weather_data$dtime_utc ,"%H"))

# pv_data <- read.csv2("Data/Table_AUTO2.csv") # automatic download in UTC
# pv_data$dtime_utc   <- as.POSIXct(pv_data$Dates, format ="%Y-%m-%d %H:%M", tz = "UTC")
# pv_data$Year   <- as.numeric(format(pv_data$dtime_utc ,"%Y"))
# pv_data$Month  <- as.numeric(format(pv_data$dtime_utc ,"%m"))
# pv_data$Day    <- as.numeric(format(pv_data$dtime_utc ,"%d"))
# pv_data$Hour   <- as.numeric(format(pv_data$dtime_utc ,"%H"))
# data_fin <- merge(pv_data,weather_data, by = c("Year","Month","Day","Hour"))
#write.csv2(data_fin, "Data/data_fin_regions.csv")

# pv_data_hour <- aggregate(pv_data[, colnames(pv_data) %in% 
#                                     c(paste(rep(c("DA_LoadFactor","ID_LoadFactor","LoadFactor"), each=14), regions,sep="_"), "Year","Month","Day","Hour")],
#                           by=list(pv_data$Year,pv_data$Month,pv_data$Day,pv_data$Hour),"mean")
# pv_data_hour <- pv_data_hour[, !colnames(pv_data_hour) %in% paste("Group", 1:4, sep=".")]
# data_fin_hour <- merge(pv_data_hour,weather_data, by = c("Year","Month","Day","Hour"))
#write.csv2(data_fin_hour,"Data/data_fin_hour_regions.csv")
