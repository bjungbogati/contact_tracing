library(shiny)
library(dplyr)
library(leaflet)

ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  sidebarLayout(
    sidebarPanel(
      actionButton(inputId = "my_button1",
                   label = "Use leafletProxy()"),
      actionButton(inputId = "my_button2",
                   label = "Use renderLeaflet()")
    ),
    mainPanel(
      leafletOutput(
        outputId = "mymap",
        width = "100%",
        height = "300px"
      ))
  ))

server <- function(input, output, session) {
  
  some_data <- data.frame(
    lon = c(4.905167, 4.906357, 4.905831),
    lat = c(52.37712, 52.37783, 52.37755),
    number_var = c(5, 9, 7),
    name = c("Jane", "Harold", "Mike"),
    stringsAsFactors = FALSE
  )
  
  marker_js <- JS("function(cluster) {
                  var html = '<div style=\"background-color:rgba(77,77,77,0.5)\"><span>' + cluster.getChildCount() + '</div><span>'
                  return new L.DivIcon({html: html, className: 'marker-cluster'});
}")
  
  output$mymap <- renderLeaflet({
    
    leaflet(some_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(
        ~min(lon),
        ~min(lat),
        ~max(lon),
        ~max(lat)
      ) %>%
      addMarkers(
        layerId = "mylayer",
        clusterId = "mycluster",
        lng = ~lon,
        lat = ~lat,
        clusterOptions = markerClusterOptions(
          iconCreateFunction = marker_js
        )
      )
    
  })
  
  observeEvent(input$my_button1, {
    
    leafletProxy("mymap", data = some_data) %>%
      removeMarker(layerId = "mylayer") %>%
      clearTiles %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(
        ~min(lon),
        ~min(lat),
        ~max(lon),
        ~max(lat)
      ) %>%
      addMarkers(
        layerId = "mylayer",
        clusterId = ~name,
        lng = ~lon,
        lat = ~lat,
        clusterOptions = markerClusterOptions(
          iconCreateFunction = marker_js
        )
      )
    
  })
  
  observeEvent(input$my_button2,{
    
    output$mymap <- renderLeaflet({
      
      leaflet(some_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        fitBounds(
          ~min(lon),
          ~min(lat),
          ~max(lon),
          ~max(lat)
        ) %>%
        addMarkers(
          layerId = "mylayer",
          clusterId = "mycluster",
          lng = ~lon,
          lat = ~lat,
          clusterOptions = markerClusterOptions(
            iconCreateFunction = marker_js
          )
        )
    })
    
  })
  
}

shinyApp(ui = ui, server = server)