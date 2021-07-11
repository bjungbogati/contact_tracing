library(shiny)
library(leaflet)
library(RColorBrewer)
library(sf)
library(jsonlite)

# addResourcePath(prefix = 'pics', directoryPath = '~/pictures')

# setwd("/home/nepal_app/")

# local_data <- readRDS("local_data.rds")
# # readr::write_csv(local_data, "local_data.csv")
# local_data$time <- 1:nrow(local_data)

district <- readRDS("./data/district.rds")


district_labels <- as.list(paste(
  "<b>District:</b>", toupper(district$district), "<br>" # Format nicer
  # "<b>Total Cases:</b>", district$confirmed, "<br>"
) %>% lapply(htmltools::HTML))

# bhk_high <- local_data %>%  dplyr::filter(condition == "High")
# bhk_mild <- local_data %>%  dplyr::filter(condition == "Mild")
# bhk_low <- local_data %>% dplyr::filter(condition == "green")

# pal <- colorFactor(pal = c("#28a745", "#007bff", "#ffc107", "#dc3545"), domain = c("green", "blue", "yellow", "red"))


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}",
             ".leaflet .legend i{
      border-radius: 50%;
      width: 10px;
      height: 10px;
      margin-top: 4px;
      }",
  "#shiny-disconnected-overlay {
             background-color: inherit;
             opacity: 0;
             }"
      ),
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
  
  # reader <- reactiveFileReader(intervalMillis = 10, 
  #                              filePath = './data/local_data.rds', 
  #                              readFunc = readRDS)
  
  # liveish_data <- reactive({
  #   invalidateLater(1000 * 60 * 30)
  #   source("./stream.R")
  # })
  
  local_data <- readRDS("./data/local_data.rds")  
  # local_data <- local_data[1:2000, ]
  
  pop <- as.list(paste(
    "<b>Person:</b>", local_data$giv_name, local_data$fam_name,"<br>", # Format nicer
    "<b>Contact:</b>", local_data$contact, "<br>", 
    "<b>Time:</b>", local_data$timestamp, "<br>"
    # local_data$latitude, local_data$longitude
  ) %>% lapply(htmltools::HTML))
  
  # Reactive expression for the data subsetted to what the user selected
  # filteredData <- reactive({
  #   local_data[local_data$ >= input$range[1] & local_data$time <= input$range[2], ]
  # })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  # colorpal <- reactive({
  #   colorNumeric(input$colors, local_data$time)
  # })
  
  output$map <- renderLeaflet({
    
  
    # 
    # addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
    #   colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
    #   labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
    #   
    #   return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
    # }
    
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(local_data) %>% addTiles(group= "Street Map") %>%
      
      leaflet::setView(lat = 28.3949, 
                       lng = 84.1240, 
                       zoom = 7)  %>% 
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
    
    leaflet::addCircles(
      lng = ~longitude,
      lat = ~latitude,
      weight = 2, 
      radius = 0.2,
      fillOpacity = 0.5,
      label = pop, 
      popup =  pop,
      popupOptions = popupOptions(style = list(
        "font-size" = "15px",
        "border-color" = "rgba(0,0,0,0.5)")),
      labelOptions = labelOptions(style = list(
                                    # "color" = "red",
                                    # "font-family" = "serif",
                                    # "font-style" = "italic",
                                    # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                    "font-size" = "15px",
                                    "border-color" = "rgba(0,0,0,0.5)")),
                                    
      group = "green",  
      color = dplyr::case_when(
        local_data$condition == "green" ~ "#28a745",
        local_data$condition == "blue" ~ "#007bff", 
        local_data$condition == "yellow" ~ "#ffc107", 
        local_data$condition == "red" ~ "#dc3545"
        )
      
      ) %>%
      
      # addLegend(pal=pal, values= factor(local_data$condition), opacity=1, na.label = "Not Available") %>%
      
      addLegend(colors = c("#28a745", "#007bff", "#ffc107", "#dc3545"), 
                labels = c("Symptoms free with no travel history", 
                           "Symptoms free with travel history", 
                           "Fever (100F°) & cough lasting beyond 48hrs", 
                           "Shortness of Breath with fever and cough"),
                opacity=1, na.label = "Not Available", 
                position = "bottomleft") %>%
     
     leaflet::hideGroup(c("Streets Map")) %>% 
      
      # addLegendCustom(colors = c("green", "blue", "yellow",  "red"), labels = c("A", "B", "C", "D"), sizes = 5) %>% 
      
      leaflet::addLayersControl(baseGroups = c("Offline", "Street Map"), #, "Aged 60+"
         
                                options = leaflet::layersControlOptions(collapsed=F))
    # 
    # fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
}

shinyApp(ui, server)

# addResourcePath('shared', system.file('shared', package='datasets'))

# addResourcePath(prefix = 'Proj4Leaflet-1.0.1', directoryPath = '/www')
# 
# lapply(names(resourcePaths()), addResourcePath(prefix = names(resourcePaths()), directoryPath = '/www'))
# 
# names(resourcePaths())
# 
# 
#