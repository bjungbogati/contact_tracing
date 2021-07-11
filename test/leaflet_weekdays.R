library(shiny)
library(leaflet)

# c("Monday","Tuesday","Wednesday", "Thursday", "Friday","Saturday","Sunday")
# 
# mydata <- data.frame(occurrencedayofweek = c(1:7),
#                      longitude = c(10.47, 10.48, 10.49, 10.50,10.51, 10.52, 10.53),
#                      latitude = c(45.76, 45.77, 45.78, 45.79, 45.80, 45.81, 45.82))


district <- readRDS("./data/district.rds")


district_labels <- as.list(paste(
  "<b>District:</b>", toupper(district$district), "<br>" # Format nicer
  # "<b>Total Cases:</b>", district$confirmed, "<br>"
) %>% lapply(htmltools::HTML))


local_data <- readRDS("./data/local_data.rds")  
# local_data <- local_data[1:2000, ]

pop <- as.list(paste(
  "<b>Person:</b>", local_data$giv_name, local_data$fam_name,"<br>", # Format nicer
  "<b>Contact:</b>", local_data$contact, "<br>", 
  "<b>Time:</b>", local_data$timestamp, "<br>"
  # local_data$latitude, local_data$longitude
) %>% lapply(htmltools::HTML))


  
  ui <- fluidPage(
    
    leafletOutput ("map"),
    
    # checkboxGroupInput("checkGroup", "Week Day",
    #                    choices = c("Monday" = 1,
    #                                "Tuesday" = 2,
    #                                "Wednesday" = 3,
    #                                "Thursday" = 4,
    #                                "Friday" = 5,
    #                                "Saturday" = 6,
    #                                "Sunday" = 7),
    #                    selected = 1)
    
    checkboxGroupInput("condition","Conditions", choices = 
                         c("Green" = "green", 
                           "Blue" = "blue",
                           "Yellow" = "yellow",
                           "Red" = "red")
                       )
    
    
  )
  
  server <- function(input, output, session){
    
    output$map <- renderLeaflet({
      leaflet() %>% 
        addTiles(group= "Street Map") %>%
        leaflet::setView(lat = 28.3949, 
                         lng = 84.1240, 
                         zoom = 7)  %>% 
        
        leaflet::addPolygons(data = district,
                             opacity = 1,
                             weight = 0.5,
                             label = district_labels, 
                             color = "#999",
                             dashArray = "0.5",
                             group = "mygroup",
                             # fillColor = c("red", "green", "blue", "orange"), 
                             smoothFactor = 0.5) %>% 
        
        addLegend(colors = c("#28a745", "#007bff", "#ffc107", "#dc3545"), 
                  labels = c("Symptoms free with no travel history", 
                             "Symptoms free with travel history", 
                             "Fever (100F°) & cough lasting beyond 48hrs", 
                             "Shortness of Breath with fever and cough"),
                  opacity=1, na.label = "Not Available", 
                  position = "bottomleft") 
      
      # 
      # leaflet() %>%
      #   addTiles() %>%
      #   addMarkers(data = mydata,
      #              ~longitude,
      #              ~latitude,
      #              group = "mygroup")
    })
    
    
    health_condition <-reactive({ local_data %>% filter(condition %in% input$condition) })
    
    # 
    # mydata_filtered <- reactive(mydata[mydata$occurrencedayofweek %in% input$checkGroup, ])
    
    observe({
      leafletProxy("map") %>%
        clearGroup ("mygroup") %>%
        
        addCircleMarkers(
          data = health_condition(), 
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
          
          group = "mygroup",

          color = dplyr::case_when(
            health_condition()$condition == "green" ~ "#28a745",
            health_condition()$condition == "blue" ~ "#007bff", 
            health_condition()$condition == "yellow" ~ "#ffc107", 
            health_condition()$condition == "red" ~ "#dc3545"
          )
        ) %>% 
        
        
            
    })
    
  }
  

  shinyApp(ui, server)
    