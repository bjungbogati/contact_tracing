library(shiny)
library(leaflet)
library(dplyr)

# Generating dummy data for demonstration
member <- 1:10
lat <- c(39.8, 39.6, 39.7, 39.78, 39.82, 39.74, 39.72, 38.9, 37.43, 38.0)
lon <- c(-86.1, -86.2, -86.3,-86.4,-86.5,-86.6,-86.7,-86.8,-86.9, -87)
group <- c("a","a","a","b","b","a","a","a","b","b")
year <- c(1,0,0,1,0,1,0,0,1,0)
data <- data.frame(member, lat, lon, group, year)

# Creating data subsets for plotting
groupA_y1 <- data %>% filter(group == "a", year == 1)
groupA_y0 <- data %>% filter(group=="a", year == 0)
groupB_y1 <- data %>% filter(group=="b", year == 1)
groupB_y0  <-data %>% filter(group=="b", year == 0)

ui <- fluidPage(
  leafletOutput("mymap"),
  checkboxInput("group", "Group:", c("A", "B"), selected = "A"),
  checkboxInput("year", "Year", c(1,0), selected = 1)
)


server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", 
                       options = providerTileOptions(noWrap = TRUE)) %>%
      setView(lng = -85.00, lat = 39.00, zoom = 6)
  })
  
  
  zerg <-reactive({
    test<-ifelse(input$group=="A" & input$year==1, return(groupA_y1),
                 ifelse(input$group=="A" & input$year==0, return(groupA_y0),
                        ifelse(input$group=="B" & input$year==1, return(groupB_y1),
                               return(groupB_y0))))
    return(test)
  })
  
  
  
  observe({
    dataset<- zerg()
    
    leafletProxy("mymap", data = dataset) %>%
      clearMarkers() %>%
      addCircleMarkers(~lon, ~lat, layerId=~member,
                       stroke=FALSE, fillOpacity=0.9, fillColor="Red") 
    
  })
}

shinyApp(ui, server)
