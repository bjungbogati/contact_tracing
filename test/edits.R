library(shiny)
library(leaflet)
library(RColorBrewer)
library(sf)
library(jsonlite)

# setwd("/home/nepal_app/")

# local_data <- readRDS("local_data.rds")
# # readr::write_csv(local_data, "local_data.csv")
# local_data$time <- 1:nrow(local_data)

district <- readRDS("./data/district.rds")

local_data <- readRDS("./data/local_data.rds")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
  # absolutePanel(top = 10, right = 10,
  #               sliderInput("range", "Time", min(local_data$time), max(local_data$time),
  #                           value = range(local_data$time), step = 0.1
  #               ),
  #               selectInput("colors", "Color Scheme",
  #                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
  #               ),
  #               checkboxInput("legend", "Show legend", TRUE)
  # )
)


server <- function(input, output, session) {
 
  
  # Reactive expression for the data subsetted to what the user selected
  # filteredData <- reactive({
  #   local_data[local_data$time >= input$range[1] & local_data$time <= input$range[2], ]
  # })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  # colorpal <- reactive({
  #   colorNumeric(input$colors, local_data$time)
  # })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(local_data) %>% addTiles(group= "Street Map") %>%
      
      leaflet::setView(lat = 28.3949, 
                       lng = 84.1240, 
                       zoom = 6)  %>% 
      leaflet::addPolygons(data = district,
                           opacity = 1,
                           weight = 0.5,
                           color = "#999",
                           dashArray = "0.5",
                           # fillColor = c("red", "green", "blue", "orange"), 
                           smoothFactor = 0.5
      )
     
      # fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    # pal <- colorpal()
    
    leafletProxy("map", data = local_data) %>%
      # clearShapes() %>%
      
      leaflet::addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 2,
        clusterOptions = leaflet::markerClusterOptions(),
        group = "High",
        color = "red"
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = local_data)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~time
      ) %>% 
        
        leaflet::hideGroup(c("Streets Map")) %>% 

        
        leaflet::addLayersControl(baseGroups = c("Offline", "Street Map"), #, "Aged 60+"
                                  
                                  position = "bottomleft",
                                  options = leaflet::layersControlOptions(collapsed=F))
    }
  })
}

shinyApp(ui, server)
