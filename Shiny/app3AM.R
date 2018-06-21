
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
library(plotly)
library(pipeR)
library(tidyr)
library(dplyr)

#source('~/ProjetCepeVolt/2. Recup modele fonction v2.R', encoding = 'UTF-8')

# Récupération table métric par modèle ... un peu long
#reglin <- recup_modele2("reglin", liste_fichiers)
#lasso <- recup_modele2("lasso", liste_fichiers)
#ridge <- recup_modele2("ridge", liste_fichiers)
#cart <- recup_modele2("cart", liste_fichiers)
#knn <- recup_modele2("knn", liste_fichiers)#

#all_mod <- rbind(reglin, lasso, ridge, cart, knn)

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

liste_fichiers <- as.list(list.files("RDS"))

# identification des modèles linéaires
sel_reglin <- grepl("^reglin", liste_fichiers)
liste_reglin <- liste_fichiers[sel_reglin]

# identification des modèles ridge
sel_ridge <- grepl("^ridge", liste_fichiers)
liste_ridge <- liste_fichiers[sel_ridge]

# identification des modèles lasso
sel_lasso <- grepl("^lasso", liste_fichiers)
liste_lasso <- liste_fichiers[sel_lasso]

# identification des modèles CART
sel_cart <- grepl("^cart", liste_fichiers)
liste_cart <- liste_fichiers[sel_cart]

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
                 selected = "Ieff"),
     
     # Select metric
     selectInput("metric", "Metric:",
                 choices = c("RMSE"="rmse",
                             "MAE" ="mae"),
                 selected = "rmse"),
     
     # Select modele
     selectInput("model", "Modèle:",
                 choices = c("Lineaire" = "reglin2",
                             "Ridge" = "ridge2",
                             "Lasso" = "lasso2",
                             "KNN" ="knn2", 
                             "CART"="cartcp2"))
      
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
               plotlyOutput("scattermod"),
               br(),
               htmlOutput("summary1"),
               br(),
               plotOutput("gmetric"),
               br(),
               plotlyOutput("gmetric_ly")
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

  # récupérer le rds du modèle retenu
   modele <- reactive({
     readRDS(paste(path, "/RDS/", input$model, ".", input$metric, "_", region(), ".rds", sep = ""))
   })
  
  # récupération des metrics
  rmse <- reactive({
    
    if (input$model %in% c("reglin2", "cartcp2", "knn2")) {
      tmp <- modele()$results$RMSE[which(modele()$results[, 1] == as.numeric(modele()$bestTune))]
    }
    
    if (input$model %in% c("ridge2", "lasso2")) {
      tmp <- modele()$results$RMSE[which(modele()$results[, 2] == as.numeric(modele()$bestTune[2]))]
    }
    
    tmp
    
  })
  
  mae <- reactive({
    
    if (input$model %in% c("reglin2", "cartcp2", "knn2")) {
      tmp <- modele()$results$MAE[which(modele()$results[, 1] == as.numeric(modele()$bestTune))]
    }
    
    if (input$model %in% c("ridge2", "lasso2")) {
      tmp <- modele()$results$MAE[which(modele()$results[, 2] == as.numeric(modele()$bestTune[2]))]
    }
    
    tmp
    
  })
  
  res_mod <- reactive({
    filter(all_mod, region == region())
  })
  
  output$summary1 <- renderPrint({
    paste(paste("Metrics du modèle", input$model), paste("RMSE = ", round(rmse(), 1)), paste("MAE = ", round(mae(), 1)), sep = "<br>")
    })
   
   output$gmetric <- renderPlot({
     data.frame(metric = c("rmse", "mae"), value = c(rmse(), mae())) %>% 
       ggplot(aes(x = metric, y = value, fill = metric)) +
       geom_bar(stat="identity", position = position_dodge()) +
       geom_text(aes(label = round(value, 1)), vjust = -.5, #color="white",
                 position = position_dodge(0.9), size = 3.5)
   })
   
   output$gmetric_ly <- renderPlotly({
     plot_ly(data = data.frame(metric = c("rmse", "mae"), value = c(rmse(), mae())),
             y = ~value, 
             x = ~metric,
             type = "bar",
             colors = "Blues") %>%
       layout(title = paste("Résultats modèle", input$model, 
                            "for", region(), sep = " "))
   })
   
   output$scattermod <- renderPlotly({
     plot_ly(data = res_mod(),
             y = ~mae, 
             x = ~rmse,
             type = "scatter",
             colors = as.factor(res_mod()$nom_model)
             ) %>%
       layout(title = paste("Comparaison modèles for", region(), sep = " "))
   })
   
  # output$click <- renderPrint({
  #   d <- event_data(event = "plotly_click", source = "select")
  #   if (is.null(d)) "Choisir un modèle" else d
  # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
