
############################
# SHINY PJ certificat
############################

path <- "C:/Users/amutter/Documents/ProjetCepeVolt/"

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
      
      # Aggregation for boxplot           
      selectInput("var", "Agrégation:",
                  c("Month" = "month",
                    "Hour" = "hour")),
      
      # Select date range
      dateRangeInput("date", strong("Date range"),
                     start = "2012-12-31", end = "2018-02-28",
                     min = "2012-12-31", max = "2018-02-28"),
      
      # Seclect variable to describe
      selectInput("expl", "Variable explicative:",
                 choices = c("Ieff", "proj", "temperature", "humidity", "cloudCover", "visibility"),
                 selected = "Ieff")
      
    ),
    
    # Show the map
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
      # Map
      leafletOutput("mymap"),
      
      fluidRow(
        fluidRow(
          column(12, plotOutput("mybp"))
        ),
        fluidRow(
            column(4, plotOutput("bpexpl1")),
            column(4, plotOutput("bpexpl2")),
            column(4, plotOutput("bpexpl3")),
            column(4, plotOutput("bpexpl4")),
            column(4, plotOutput("bpexpl5")),
            column(4, plotOutput("bpexpl6"))
          )
        )
      ),
      
      tabPanel("Scatterplot",
        plotOutput("scatter")
      ),
      
      tabPanel("Modèle",
               verbatimTextOutput("summary1")
      )
      
      )
      
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
  
  region <- reactive({
    input$mymap_shape_click$id
  })
  
  formulaText <- reactive({
    paste(paste(input$profile,region(),sep="_"), "~", input$var) 
    # a modifier en fonction de la région choisie en cliquant :)
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
  
  output$mybp <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = donnees(),
            col = "lightblue", pch = 19,
            outline = FALSE,
            main = paste("Boxplots per", input$var, 
                         "for", input$profile, 
                         "for", region(), sep= " "))
  })
  
  output$bpexpl1 <- renderPlot({
    boxplot(as.formula(paste("Ieff ~", input$var)),
            data = donnees(),
            col = "lightblue", pch = 19,
            main = "Ieff")
  })

  output$bpexpl2 <- renderPlot({
    boxplot(as.formula(paste("proj ~", input$var)),
            data = donnees(),
            col = "lightblue", pch = 19,
            main = "proj")
  })

  output$bpexpl3 <- renderPlot({
    boxplot(as.formula(paste("temperature ~", input$var)),
                        data = donnees(),
                        col = "lightblue", pch = 19,
                        main = "temperature")
  })
    
  output$bpexpl4 <- renderPlot({
    boxplot(as.formula(paste("humidity ~", input$var)),
                        data = donnees(),
                        col = "lightblue", pch = 19,
                        main = "humidity")
  })
    
  output$bpexpl5 <- renderPlot({
    boxplot(as.formula(paste("cloudCover ~", input$var)),
                        data = donnees(),
                        col = "lightblue", pch = 19,
                        main = "cloudCover")
  })
    
  output$bpexpl6 <- renderPlot({
    boxplot(as.formula(paste("visibility ~", input$var)),
                        data = donnees(),
                        col = "lightblue", pch = 19,
                        main = "visibility")
  })
  
  output$scatter <- renderPlot({
    plot(as.formula(paste(input$expl, "~", paste(input$profile,region(),sep="_"))),
         data = donnees(),
         pch = ".")
  })
  
#-----------------------------------
#Modèle
#-----------------------------------
  mod1 <- reactive({
    lm(as.formula(paste(paste(input$profile,region(),sep="_"), "~ Ieff:hour+Ieff:cloudCover:hour+hour+Ieff:humidity:hour-1")),
       data = donnees())
  })
  
  output$summary1 <- renderPrint({
    summary(mod1())
    })
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
