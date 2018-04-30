
#install.packages("sp")
#install.packages("BelgiumMaps.StatBel", repos = "http://www.datatailor.be/rcube", type = "source")

library(shiny)
library(leaflet)
library(BelgiumMaps.StatBel)
library(lubridate)

data(BE_ADMIN_PROVINCE)
data(BE_ADMIN_REGION)

#BE_ADMIN_REGION@data$TX_RGN_DESCR_FR
#BE_ADMIN_PROVINCE@dataTX_PROV_DESCR_FR

#Sera importé de la base de données météo
villes <- data.frame(Ville = c("Zaventem", "Bierset", "Chièvres", "Deurne", "Elsenborn", "Florennes", "Gent", "Kleine Brogel", 
                               "Oostende", "Saint-Hubert", "Virton"),
                     Latitude = c(50.8833, 50.655,50.5879, 51.2167, 50.4806, 50.25, 51.05, 51.1667, 51.2167,50.0167,49.5667 ),
                     Longitude = c(4.4667, 5.45092, 3.8071, 4.4167, 6.2514, 4.6167, 3.7167, 5.4333, 2.9167, 5.3833, 5.5333))

#donnees PV par region
data <- read.csv2("D:/Certificat Data Scientist/PJ/Data/Table_Auto2.csv")
data$dtime_utc  <- as.POSIXct(data$Dates, format ="%Y-%m-%d %H:%M", tz = "UTC")
data$month <- month(data$dtime_utc)
data$hour <- hour(data$dtime_utc)

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
    m <- addMarkers(m, lng = ~Longitude, lat = ~Latitude, popup = ~Ville)
    m <- addPolylines(m, data = BE_ADMIN_PROVINCE, weight = 1.5, color = "black")
    m <- addPolygons(m, data = BE_ADMIN_PROVINCE, layerId = BE_ADMIN_PROVINCE@data$TX_PROV_DESCR_FR, 
                     fillColor = topo.colors(11, alpha = NULL), stroke = FALSE)
    m
  })
  
  formulaText <- reactive({
    paste(input$profile, "~", input$var) # a modifier en fonction de la région choisie en cliquant :)
  })

  output$mybp <- renderPlot({
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

