library(shiny)
library(shinyjs)
library(leaflet)

cords <- data.frame(
  lng = runif(100, 14, 18),
  lat = runif(100, 54, 58),
  circle_pt = sample(1:20, size = 100, replace = T)
)

ui <- fluidPage(
  leafletOutput("map", height = "700px")
) 

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(data = cords) %>% 
      addTiles() %>% 
      addCircleMarkers(lat = ~lat,lng = ~lng, layerId = ~circle_pt, fillColor = 'green',
                       opacity = 0.5,color = 'red',fillOpacity = 1) 
  })
  
  observeEvent(input$map_marker_click, {
    clickid = input$map_marker_click$id
    cordsNew = cords[cords$circle_pt==clickid,]
    
    maxLong = max(cordsNew$lng)
    maxLat = max(cordsNew$lat)
    minLong = min(cordsNew$lng)
    minLat = min(cordsNew$lat)
    
    proxy <-leafletProxy("map", data = cordsNew)
    proxy %>%
      addCircleMarkers(lat = ~lat,lng = ~lng, layerId = ~circle_pt, fillColor = 'green',
                       opacity = 0.5,color = 'red',fillOpacity = 1) %>% 
      
      fitBounds(minLong,minLat,maxLong,maxLat) %>% 
      
      clearPopups() %>%
      addPopups(lat=~lat,lng=~lng,~as.character(circle_pt))
  })
}

shinyApp(ui = ui, server = server)