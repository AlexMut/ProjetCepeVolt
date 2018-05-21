#####################################
#       Essais de modèle
#####################################
library(lubridate)

regions <- c("Brussels","Antwerp","Flemish.Brabant","Walloon.Brabant",
  "West.Flanders","East.Flanders","Hainaut", 
  "Liège","Limburg","Luxembourg","Namur")

r <- "Limburg"

df <- read.csv2(paste("Data/db_fin_",r,".csv",sep=""),stringsAsFactors = FALSE)
names(df)[2:4]<-c("LF","DA_LF","ID_LF")
df$dtime_utc  <- as.POSIXct(df$dtime_utc, format ="%Y-%m-%d %H:%M", tz = "UTC")
df$month <- as.factor(month(df$dtime_utc))
df$hour <- as.factor(hour(df$dtime_utc))
df$precipType <- as.factor(df$precipType)

mat_cor <- cor(df[,c("LF","temperature","apparentTemperature","dewPoint","humidity", 
                     "pressure","windSpeed","cloudCover", "visibility","Ieff","proj")],
               use = "complete.obs")
barplot(mat_cor[1,-1], main = paste("Correlations between data in",r,sep=" ") ,ylim=c(-1,1), las=2)

#reg lin
summary(lm(LF~Ieff-1, data=df))$adj.r.squared #0.5362016
summary(lm(LF~Ieff:hour+hour-1, data=df))$adj.r.squared #0.7092862
summary(lm(LF~Ieff:hour+Ieff:cloudCover:hour+hour-1, data=df))$adj.r.squared #0.7598609
summary(lm(LF~Ieff:hour+Ieff:cloudCover:hour+hour+Ieff:humidity:hour-1, data=df))$adj.r.squared #0.7729731
summary(lm(LF~Ieff:hour+Ieff:cloudCover:hour+hour+Ieff:humidity:hour-1, data=df))$adj.r.squared #0.7729731

#nn

#svr

#
