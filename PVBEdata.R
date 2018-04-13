# Belgium data

# Collecte des données sur le site d'Elia #----------------

library('RCurl')
library('jsonlite')
opts <- list(proxy="proxygin.melinda.local", proxyport=8080)        

histo <- seq(as.Date("2013-01-01"),as.Date("2018-01-10"), by="month")
pv_data <- NULL
for (m in 1: (length(histo)-1))
{
  name.url <- paste("http://publications.elia.be/Publications/publications/solarforecasting.v3.svc/GetChartDataForZone?dateFrom=", histo[m], "&dateTo=",  histo[m+1], "&sourceId=", 1, sep="")
  json <- getURL(name.url,.opts=opts)
  data <- fromJSON(json)
  donnees <- cbind(data$SolarForecastingChartDataForZoneItems$StartsOn[1],
                   data$SolarForecastingChartDataForZoneItems[c("Forecast","ForecastUpdated","LoadFactor", "RealTime")])
  donnees$Dates <- as.POSIXct(as.numeric(substr(donnees$DateTime,7,19))/1000, origin = "1970-01-01",tz = "UTC")
  pv_data <- rbind(pv_data, donnees)
}
pv_data <- pv_data[which(duplicated(pv_data)=="FALSE"),]

#add capacity
json  <- getURL("http://publications.elia.be/Publications/publications/solarforecasting.v3.svc/GetCapacities?dateFrom=2014-01-01&dateTo=2016-01-31&sourceId=1",.opts=opts)
data  <- fromJSON(json)
elia_before <- data$Power

json  <- getURL("http://publications.elia.be/Publications/publications/solarforecasting.v3.svc/GetCapacities?dateFrom=2016-01-01&dateTo=2016-01-31&sourceId=1",.opts=opts)
data  <- fromJSON(json)
elia_2015 <- data$Power

capa_r <- ifelse(pv_data$Dates <="2015-09-09 22:00", elia_before, elia_2015)
pv_data$DA_LoadFactor <- pv_data$Forecast/capa_r*100
pv_data$ID_LoadFactor <- pv_data$ForecastUpdated/capa_r*100
write.csv2(pv_data,"Data/Table_Auto_BE.csv", row.names=FALSE)

