
############################
# SHINY PJ certificat
############################

#install.packages("BelgiumMaps.StatBel", repos = "http://www.datatailor.be/rcube", type = "source")

library(shiny)
library(leaflet)
library(BelgiumMaps.StatBel)
library(lubridate)
library(sp)
library(raster)
library(plotly)
library(pipeR)
library(tidyr)
library(dplyr)
library(shinyjs)
library(data.table)
library(insol)
library(lubridate)

library(RCurl)
library(jsonlite)

# Modifier le shapefiles des données
#-----------------------------------
data(BE_ADMIN_PROVINCE) # Pour les provinces
data(BE_ADMIN_REGION)   # Pour les régions
data(BE_ADMIN_BELGIUM)  # Pour le pays entier

#homogénéiser les noms avec le reste du projet
BE_ADMIN_PROVINCE@data$TX_PROV_DESCR_FR <- c("Antwerp","Flemish.Brabant","Walloon.Brabant",
                                             "West.Flanders","East.Flanders","Hainaut", 
                                             "Liège","Limburg","Luxembourg","Namur")
BE_ADMIN_PROVINCE@data$zones <- BE_ADMIN_PROVINCE@data$TX_PROV_DESCR_FR

BE_ADMIN_REGION@data$TX_RGN_DESCR_FR <- c("Flanders","Wallonia","Brussels")
BE_ADMIN_REGION@data$zones <- BE_ADMIN_REGION@data$TX_RGN_DESCR_FR

BE_ADMIN_BELGIUM@data$zones <- BE_ADMIN_BELGIUM@data$TX_RGN_DESCR_FR <- "Belgium"

#rajouter la couche de bruxelles pour les Provinces
BE_ADMIN_PROVINCE <- bind(BE_ADMIN_PROVINCE,BE_ADMIN_REGION[-(1:2),], keepnames=FALSE)

# Import des coordonnées des stations météo avec leur zone
#-----------------------------------
villes <- read.csv2("Data/VillesBelReg2.csv")

# Import & modif des fichiers de données initiaux
#----------------------------------
read_fichiers <- function(r){
  fich <- read.csv2(paste("Data/db_fin_", r,".csv",sep=""),stringsAsFactors = FALSE)
  fich$dtime_utc  <- as.POSIXct(fich$dtime_utc, format ="%Y-%m-%d %H:%M", tz = "UTC")
  fich$month <- month(fich$dtime_utc)
  fich$hour <- hour(fich$dtime_utc)
  return(as.data.frame(fich))
}

regions <- c("Antwerp","Flemish.Brabant","Walloon.Brabant",
             "West.Flanders","East.Flanders","Hainaut","Limburg","Luxembourg","Namur",
             "Flanders","Wallonia","Brussels","Belgium","Liège")
datas <- list()
for (r in regions) {datas[[paste("db",r,sep="_")]]<-read_fichiers(r)}

#Import des résultats des modèles
res_models <- read.csv2("Models/model_results.csv", dec=".")

#Préparation du temps réel
#source("4_ASourcerPourLeTempsReel.R")

# PROGRAMME PRINCIPAL
#--------------------
ui <- fluidPage(
  
  # Application title
  titlePanel("Belgian PhotoVoltaic Production"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # Zone choice
      selectInput("zone", "Zone: ",
                  c("Country" = "country",
                    "Regions"= "regions",
                    "Provinces"= "provinces")),
      
     p(div(textOutput("chosen_zone"), style="color:red")),

      # Aggregation for boxplot           
      selectInput("var", "Aggregation:",
                  c("Month" = "month",
                    "Hour UTC" = "hour")),
      
      # Select Dependent Variable
      selectInput("expl", "Dependent Variable:",
                 choices = c("Effective Irradiance"="Ieff",
                             "Angle of incidence projection" ="proj", 
                             "Temperature"="temperature",
                             "Humidity" = "humidity", 
                             "Cloud Cover"="cloudCover", 
                             "Visibility"="visibility"),
                 selected = "Ieff"),
     
     # Select criterium
     selectInput("crit", "Optimization criterium:",
                 choices = c("RMSE"="RMSE",
                             "MAE" ="MAE"),
                 selected = "rmse"),
     
     actionButton("bouton", "Click on the button for real time forecasting")
     
    ),
    
    # Show the map
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
                 leafletOutput("mymap"),
                 br(),
                 fluidRow(
                   column(6, plotlyOutput("mybply")),
                   column(6, plotlyOutput("myheatmaply"))
                 )
        ),
        
      tabPanel("Models",
               fluidRow(
                 column(6, plotlyOutput("scattermod")),
                 column(6, verbatimTextOutput("chosen_model"))
                 ),
               br(),  
               plotlyOutput("fcst_live")
               )
      )
      )
    )
)


# Server
#------
server <- function(input, output, session) {
  
  # statistics' part
  
  get_fond_carte <- reactive({
    data_carte <- list()
    if (input$zone=="country") data_carte["zone"]<- BE_ADMIN_BELGIUM
    if (input$zone=="regions") data_carte["zone"]<- BE_ADMIN_REGION
    if (input$zone=="provinces") data_carte["zone"]<- BE_ADMIN_PROVINCE
    data_carte[["villes"]]<- villes[which(villes$Zone==input$zone),]
    data_carte
  })

    output$mymap <- renderLeaflet({
    data_carte <- get_fond_carte()
    m <- leaflet(data_carte[["villes"]]) 
    m <- addTiles(m) 
    m <- addMarkers(m, lng = ~Long, lat = ~Lat, popup = ~Ville)
    m <- addPolylines(m, data = data_carte[["zone"]], weight = 1.5, color = "black")
    m <- addPolygons(m, data = data_carte[["zone"]], layerId = data_carte[["zone"]]@data$zones, 
                     fillColor = topo.colors(11, alpha = NULL), stroke = FALSE)
    m
  })
  
  region <- reactive({
    input$mymap_shape_click$id
  })
  
  formulaText <- reactive({
    paste(paste("LoadFactor",region(),sep="_"), "~", input$var) # modifié en fonction de la région choisie en cliquant :)
  })
  
  donnees <- reactive({
    req(region())
    donnees <- datas[[paste("db",region(),sep="_")]]     
    donnees
  })
  
  calculate_cor <- function(df){
    df <- na.omit(df)
    corcalc <- ifelse(!is.na(cor(df[,1],df[,2])),cor(df[,1],df[,2]),0)
  }
  
  correl <- reactive({
    df <- donnees()[,c("LoadFactor", input$expl)]
    ldf <- split(df, donnees()[,input$var])
    correl <- sapply(ldf, FUN= calculate_cor)
    correl
    })

  output$chosen_zone <- renderText(
    paste("The chosen zone is",input$mymap_shape_click$id, sep=" ")
  )
  
  output$mybply <- renderPlotly({
    plot_ly(data = donnees(),y=~LoadFactor, 
            x=as.formula(paste0("~", input$var)),type="box")%>%
      layout(title = paste("Boxplots of PV production per", input$var,
                         "for", region(), sep= " "))
  })
  
  output$myheatmaply <- renderPlotly({
    plot_ly(y=correl(), type="bar")%>%
      layout(title = paste("Correlation between PV production and",input$expl, sep= " "),
             yaxis = list(title = "Correlation", range = c(-1,1)),
             xaxis = list(title = input$var))
  })
  
  # Models'part
  
  res_mod <- reactive({
    filter(res_models, Optcrit == input$crit & region == region())
  })
  
   output$scattermod <- renderPlotly({
     plot_ly(data = res_mod(),
             y = ~MAE,
             x = ~RMSE,
             type = "scatter",
             mode = "markers",
             sizes = c(10,20),
             size = ~Rsquared,
             marker = list(opacity = 0.5, sizemode = 'diameter'),
             hoverinfo = 'text',
             text = ~paste('Model:', mod,
                           '<br>RMSE:', round(RMSE,2),
                           '<br>MAE:', round(MAE,2),
                           '<br>R²:', round(Rsquared,2)),
             color = as.factor(res_mod()$mod)
             ) %>%
       layout(title = paste("Models comparison for", region(),"\n\n",
                            "with optimization criterium",input$crit,sep = " "),
              showlegend = FALSE)
   })
   
   nom_mod <-reactive({
     s <- event_data("plotly_click")
     nom_mod <-res_mod()[s$curveNumber+1,"mod"]
     nom_mod
   })
   
   final_mod <- reactive({
     s <- event_data("plotly_click")
     nom_file <- paste("Models/",
                      res_mod()[s$curveNumber+1,"mod"],"2.",
                      tolower(input$crit), "_",
                      region(),".rds",sep="")
     final_mod <- readRDS(nom_file)
     final_mod
   })
 
   output$chosen_model <- renderPrint({
     s <- event_data("plotly_click")
     if (is.null(s)) {
       cat("Click on a bubble in the scatterplot to have details about this model")
     } else {
       cat( paste("You selected the model", res_mod()[s$curveNumber+1,"mod"],
                  "for", res_mod()[s$curveNumber+1,"region"],"\n\n",sep=" "))
       print(final_mod())
     }
    })
   
   observeEvent(input$bouton, {
     ville_choisie <- as.character(villes[villes$Région == gsub("-", ".",  region()), "Ville"])
     
     pv_heure <- aggregate(pv[, paste(c("DA_LoadFactor","ID_LoadFactor"),region(), sep="_")],
                           list(dtime_utc = pv$heure_utc), sum) #par heure
     colnames(pv_heure) <- c("dtime_utc","DA_LoadFactor","ID_LoadFactor")
     pv_heure$dtime_utc <- as.character(as.POSIXct(pv_heure$dtime_utc, format = "%Y-%m-%d %H:%M:%S"))

     Ieff_ville <- Ieff[,c("dtime_utc", ville_choisie)]
     names(Ieff_ville)<- c("dtime_utc","Ieff")
     proj_ville <- proj[,c("dtime_utc", ville_choisie)]
     names(proj_ville)<- c("dtime_utc","proj")

     # import des données météo sur la ville qui nous intéresse
     meteo <- get_meteo_now(coord_ville, ville_choisie)
     meteo$dtime_utc <- as.character(as.POSIXct(meteo$dtime_utc, format = "%Y-%m-%d %H:%M:%S"))

     # #fusion entre metéo et cities_IEff et cities_aoi_proj pour la bonne ville
     expl <- merge(x=pv_heure, y=meteo, by="dtime_utc")
     expl <-merge(x=expl, y=Ieff_ville, by="dtime_utc")
     expl <-merge(x=expl, y=proj_ville, by="dtime_utc")
     expl <- expl[,c("dtime_utc","precipType","DA_LoadFactor","ID_LoadFactor","temperature","apparentTemperature",
                     "dewPoint","humidity","pressure","windSpeed","windBearing","cloudCover",
                     "visibility","precipIntensity","precipProbability","Ieff","proj")]
     expl$precipAccumulation<-0 #is missing
     expl$month <- as.factor(month(expl$dtime_utc))
     expl$hour <- as.factor(hour(expl$dtime_utc))

     prev <- reactive({
       ypred <- predict(final_mod(), expl)
       dtime_utc <- as.POSIXct(expl$dtime_utc,tz="UTC")
       prev <- data.frame(dtime_utc,
                          cbind(ypred,
                                expl$DA_LoadFactor,
                                expl$ID_LoadFactor))
       names(prev)<- c("dtime_utc","Fcst_LoadFactor", "DA_LoadFactor","ID_LoadFactor")
       prev
     })

     output$fcst_live <- renderPlotly({
       plot_ly(data=prev(), x=~dtime_utc)%>%
         add_lines(y=~Fcst_LoadFactor, color=I("red"))%>%
         add_lines(y=~DA_LoadFactor, color=I("blue"))%>%
         add_lines(y=~ID_LoadFactor, color=I("green"))%>%
         layout(title = paste("Real Time Forecasts with weather station", ville_choisie, sep=" "),
                xaxis = list(title = "dtime_utc"),
                yaxis = list(title = "PV Production"))
     })
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
