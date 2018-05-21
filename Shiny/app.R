
############################
# SHINY PJ certificat
############################

path <- "D:/Certificat Data Scientist/PJ/ProjetCepeVolt/"

#install.packages("BelgiumMaps.StatBel", repos = "http://www.datatailor.be/rcube", type = "source")

library(shiny)
library(leaflet)
library(BelgiumMaps.StatBel)
library(lubridate)
library(sp)
library(raster)

#-----------------------------------
# Modifier le shapefiles des données
#-----------------------------------
data(BE_ADMIN_PROVINCE)
data(BE_ADMIN_REGION)

#homogénéiser les noms avec le reste du projet
BE_ADMIN_PROVINCE@data$TX_PROV_DESCR_FR <- c("Antwerp","Flemish.Brabant","Walloon.Brabant",
                                             "West.Flanders","East.Flanders","Hainaut", 
                                             "Liège","Limburg","Luxembourg","Namur")
BE_ADMIN_PROVINCE@data$zones <- BE_ADMIN_PROVINCE@data$TX_PROV_DESCR_FR

BE_ADMIN_REGION@data$TX_RGN_DESCR_FR[BE_ADMIN_REGION@data$TX_RGN_DESCR_FR=="Région de Bruxelles-Capitale"] <- "Brussels"
BE_ADMIN_REGION@data$zones <- BE_ADMIN_REGION@data$TX_RGN_DESCR_FR

#rajouter la couche de bruxelles
BE_ADMIN <- bind(BE_ADMIN_PROVINCE,BE_ADMIN_REGION[-(1:2),], keepnames=FALSE)

#-----------------------------------
# Import des coordonnées des stations météo
#-----------------------------------
villes <- read.csv2(paste(path,"Data/VillesBelReg.csv",sep=""))

#-----------------------------------
#donnees PV par region (defaut)
#-----------------------------------

ui <- fluidPage(
  
  # Application title
  titlePanel("Belgian PV Profile"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # Profile Choice
      selectInput("profile", "PV Profile:",
                  c("Actual" = "LoadFactor",
                    "Day Ahead Forecast" = "DA_LoadFactor",
                    "Intra Day Forecast" = "ID_LoadFactor")),
      
      #  Aggregation for boxplot           
      selectInput("var", "Variable:",
                  c("Month" = "month",
                    "Hour" = "hour"))
    ),
    
    # Show the map
    mainPanel(
      # Map
      leafletOutput("mymap"),
      # Boxplots 
      plotOutput("mybp")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$mymap <- renderLeaflet({
    m <- leaflet(villes) 
    m <- addTiles(m) 
    m <- addMarkers(m, lng = ~Long, lat = ~Lat, popup = ~Ville)
    m <- addPolylines(m, data = BE_ADMIN, weight = 1.5, color = "black")
    m <- addPolygons(m, data = BE_ADMIN, layerId = BE_ADMIN@data$zones, 
                     fillColor = topo.colors(11, alpha = NULL), stroke = FALSE)
    m
  })
  
  formulaText <- reactive({
    paste(paste(input$profile,input$mymap_shape_click$id,sep="_"), "~", input$var) 
    # a modifier en fonction de la région choisie en cliquant :)
  })

  output$mybp <- renderPlot({
    data <- read.csv2(paste(path,
                             paste("Data/db_fin_",input$mymap_shape_click$id,".csv",sep=""),
                             sep=""), 
                       stringsAsFactors = FALSE)
    data$dtime_utc  <- as.POSIXct(data$dtime_utc, format ="%Y-%m-%d %H:%M", tz = "UTC")
    data$month <- month(data$dtime_utc)
    data$hour <- hour(data$dtime_utc)
    
    boxplot(as.formula(formulaText()),
            data = data,
            col = "lightblue", pch = 19,
            outline = FALSE,
            main = paste("Boxplots per", input$var, 
                         "for", input$profile, 
                         "for", input$mymap_shape_click$id, sep= " "))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

