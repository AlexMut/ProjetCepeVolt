
############################
# SHINY PJ certificat
############################

path <- "E:/Certificat Data Scientist/PJ/ProjetCepeVolt/"

#install.packages("BelgiumMaps.StatBel", repos = "http://www.datatailor.be/rcube", type = "source")

library(shiny)
library(leaflet)
library(BelgiumMaps.StatBel)
library(lubridate)
library(sp)
library(raster)
library(plotly)
library(pipeR)

#-----------------------------------
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

#-----------------------------------
# Import des coordonnées des stations météo avec leur zone
#-----------------------------------
villes <- read.csv2(paste(path,"Data/VillesBelReg2.csv",sep=""))

#-----------------------------------
#donnees PV par region (defaut)
#-----------------------------------

ui <- fluidPage(
  
  # Application title
  titlePanel("Belgian PV Profile"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # Zone choice
      selectInput("zone", "Zone : ",
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
                 choices = c("Effective Irradience"="Ieff",
                             "Angle of incidence projection" ="proj", 
                             "Temperature"="temperature",
                             "Humidity" = "humidity", 
                             "Cloud Cover"="cloudCover", 
                             "Visibility"="visibility"),
                 selected = "Ieff")
      
    ),
    
    # Show the map
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
      # Map
      leafletOutput("mymap"),
      
      br(),
      
      fluidRow(
        column(6, plotlyOutput("mybply")),
        column(6, plotlyOutput("myheatmaply"))
        )
      ),

      tabPanel("Models",
               verbatimTextOutput("summary1")
      )
      
      )
      
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
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
    donnees <- read.csv2(paste(path,
                            paste("Data/db_fin_",region(),".csv",sep=""),
                            sep=""),
                      stringsAsFactors = FALSE)
    donnees$dtime_utc  <- as.POSIXct(donnees$dtime_utc, format ="%Y-%m-%d %H:%M", tz = "UTC")
    donnees$month <- month(donnees$dtime_utc)
    donnees$hour <- hour(donnees$dtime_utc)
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
  
#-----------------------------------
#Modèle
#-----------------------------------
  # load("~/ProjetCepeVolt/RMSE_Antwerp.RData")
  # mod1 <- reactive({
  #   mod.reglin$finalModel
  # })
  # 
  # output$summary1 <- renderPrint({
  #   summary(mod1())
  #   })
  # 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
