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

pop <- as.list(paste(
  "<b>Person:</b>", local_data$giv_name, local_data$fam_name,"<br>", # Format nicer
  "<b>Condition:</b>", local_data$condition, "<br>"
) %>% lapply(htmltools::HTML))

district_labels <- as.list(paste(
  "<b>District:</b>", toupper(district$district), "<br>", # Format nicer
  "<b>Total Cases:</b>", district$confirmed, "<br>"
) %>% lapply(htmltools::HTML))

bhk_high <- local_data %>%  dplyr::filter(condition == "High")
bhk_mild <- local_data %>%  dplyr::filter(condition == "Mild")
bhk_low <- local_data %>% dplyr::filter(condition == "green")

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
                           label = district_labels, 
                           color = "#999",
                           dashArray = "0.5",
                           # fillColor = c("red", "green", "blue", "orange"), 
                           smoothFactor = 0.5
      ) %>%
      
      # leaflet::addCircleMarkers(
      #   data = bhk_high,
      #   lng = ~longitude,
      #   lat = ~latitude,
      #   radius = 2,
      #   popup =  pop,
      #   clusterOptions = leaflet::markerClusterOptions(),
      #   group = "High",
      #   color = "red") %>%
      # 
      # leaflet::addCircleMarkers(
      #   data = bhk_mild,
      #   lng = ~longitude,
      #   lat = ~latitude,
      #   radius = 2,
      #   popup =  pop,
      #   clusterOptions = leaflet::markerClusterOptions(),
      #   group = "Mild",
      #   color = "yellow") %>%
      
      leaflet::addCircleMarkers(
        data = bhk_low,
        lng = ~longitude,
        lat = ~latitude,
        radius = 1,
        popup =  pop,
        clusterOptions = leaflet::markerClusterOptions(iconCreateFunction =
                                                            JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")
        ),
        group = "Low",
        color = "green") %>%
      
      leaflet::hideGroup(c("Streets Map")) %>% 
      
      leaflet::addLayersControl(baseGroups = c("Offline", "Street Map"), #, "Aged 60+"
                                overlayGroups = c("High", 
                                                  "Mild", 
                                                  "Low"), # , "Streets Map"
                                options = leaflet::layersControlOptions(collapsed=F))
     
      # fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
})
  
}

shinyApp(ui, server)