# 
# district <- readRDS("./data/district.rds")
# local_data <- readRDS("./data/local_data.rds")
# sum_data <- readRDS("./data/sum_data.rds")
# 
# local_data <- local_data %>% distinct(id, .keep_all = T)
# local_data <- local_data[sample(nrow(local_data), 1200, replace = T), ]
# 
# # Leaflet bindings are a bit slow; for now we'll just sample to compensate
# set.seed(100)

# local_data <- local_data[1:2000, ]

function(input, output, session) {
  
  ## authentication
  
  f <- FirebaseEmailPassword$new(persistence = "session")
  
  observeEvent(input$signin, {
    f$sign_in(input$email_signin, input$password_signin)
    
    showNotification("Welcome, Dear Guest", duration = 5, type = "message")
  })
  

  observeEvent(input$signout, {
    f$sign_out()
    
    showNotification("See you, next time", duration = 5, type = "message")
  })
  
  
  
  
  ## Interactive Map ###########################################
  observe({
    updateDateRangeInput(session, "date",  start  = min(local_data$date),
                         end    = max(local_data$date),
                         min    = min(local_data$date),
                         max    = max(local_data$date))
  })
  
  health_condition <- reactive({
    local_data %>% 
      filter(condition %in% input$condition, 
             between(date, input$date[1], input$date[2]),
             date >= input$timestamp[1],
             date <= input$timestamp[2]
      )
    # date >= input$date[1], 
    # date <= input$date[2])
    # time >= input$time[1], 
    # time <= input$time[2])
  })
  
  # sliderData1 <- reactive({
  #   fake_data[fake_data$Population >= input$range[1] & fake_data$Population <= input$range[2] & fake_data$Class1 == TRUE,]
  # })
  # 
  pop <- as.list(paste(
    "<b>Person:</b>", local_data$giv_name, local_data$fam_name,"<br>", # Format nicer
    "<b>Contact:</b>", local_data$contact, "<br>", 
    "<b>Time:</b>", local_data$timestamp, "<br>"
    # local_data$latitude, local_data$longitude
  ) %>% lapply(htmltools::HTML))
  
  district_labels <- as.list(paste(
    "<b>District:</b>", toupper(district$district), "<br>" # Format nicer
    # "<b>Total Cases:</b>", district$confirmed, "<br>"
  ) %>% lapply(htmltools::HTML))
  
  #valuebox
  output$obs_row <- renderText({ 
    length(unique(local_data$id))
  })
  
  output$obs_green <- renderText({ 
    local_data %>% filter(condition == "green") %>% distinct(id) %>% nrow()
  })
  
  output$obs_blue <- renderText({ 
    local_data %>% filter(condition == "blue") %>% distinct(id) %>% nrow()
  })
  
  output$obs_yellow <- renderText({ 
    local_data %>% filter(condition == "yellow") %>% distinct(id) %>% nrow()
  })
  
  output$obs_red <- renderText({ 
    local_data %>% filter(condition == "red") %>% distinct(id) %>% nrow()
  })
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(group = "Street Map") %>%
      # leaflet::setView(lat = 28.3949, lng = 84.1240, zoom = 7) %>% 
      leaflet::setView(lat = 27.6658298, lng = 85.3432408, zoom = 12) %>% 
      leaflet::addPolygons(data = district,
                           opacity = 1,
                           weight = 0.5,
                           label = district_labels, 
                           color = "#999",
                           dashArray = "0.5",
                           group = "Offline",
                           # fillColor = c("red", "green", "blue", "orange"), 
                           smoothFactor = 0.5
      ) %>%
      
      # leaflet.extras::addHeatmap( 
      #   lng = ~longitude,
      #   lat = ~latitude,
      #   blur = 20, 
      #   max = 0.05, 
      #   radius = 15,
      #   group = "heatmat") %>% 
      
      leaflet::addLayersControl(baseGroups = c("Offline", "Street Map"), #, "Aged 60+"
                                position = "bottomleft", 
                                # overlayGroups = c("heatmap"),
                                options = leaflet::layersControlOptions(collapsed=F)) %>% 
      
      leaflet::hideGroup(c("Streets Map")) %>%
      
      addLegend(colors = c("#28a745", "#007bff", "#ffc107", "#dc3545"), 
                labels = c("Symptoms free with no travel history", 
                           "Symptoms free with travel history", 
                           "Fever (100F°) & cough lasting beyond 48hrs", 
                           "Shortness of Breath with fever and cough"),
                opacity= 1, na.label = "Not Available", 
                position = "bottomright") 
    # 
    # leaflet::addCircleMarkers(
    #   data = health_condition(), 
    #   lng = ~longitude,
    #   lat = ~latitude,
    #   weight = 3, 
    #   radius = 0.2,
    #   fillOpacity = 0.5,
    #   label = pop, 
    #   popup =  pop,
    #   popupOptions = popupOptions(style = list(
    #     "font-size" = "15px",
    #     "border-color" = "rgba(0,0,0,0.5)")),
    #   labelOptions = labelOptions(style = list(
    #     # "color" = "red",
    #     # "font-family" = "serif",
    #     # "font-style" = "italic",
    #     # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
    #     "font-size" = "15px",
    #     "border-color" = "rgba(0,0,0,0.5)")),
    #   group = "green",  
    #   color = dplyr::case_when(
    #     health_condition()$condition == "green" ~ "#28a745",
    #     health_condition()$condition == "blue" ~ "#007bff", 
    #     health_condition()$condition == "yellow" ~ "#ffc107", 
    #     health_condition()$condition == "red" ~ "#dc3545"
    #   )
    # )
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  # zipsInBounds <- reactive({
  #   if (is.null(input$map_bounds))
  #     return(zipdata[FALSE,])
  #   bounds <- input$map_bounds
  #   latRng <- range(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)
  #   
  #   subset(zipdata,
  #          latitude >= latRng[1] & latitude <= latRng[2] &
  #            longitude >= lngRng[1] & longitude <= lngRng[2])
  # })
  
  # Precalculate the breaks we'll need for the two histograms
  # centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  
  # output$histCentile <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #   
  #   hist(zipsInBounds()$centile,
  #        breaks = centileBreaks,
  #        main = "SuperZIP score (visible zips)",
  #        xlab = "Percentile",
  #        xlim = range(allzips$centile),
  #        col = '#00DD00',
  #        border = 'white')
  # })
  
  # output$scatterCollegeIncome <- renderPlot({
  # 
  # })
  
  center <- reactive({
    
    if(is.null(input$bhaktapur_map)){
      return(c(179.462, -20.64275))
    }else{
      return(input$bhaktapur_map)
    }
  })
  
  
  output$contact_trace_plot <- renderPlotly({
    ggplotly(
      ggplot(data = sum_data, aes(x = date, y = count, 
                                  color = condition)) + 
        geom_line() + 
        ggthemes::theme_gdocs() +
        theme(legend.position = "none") +
        scale_color_manual(values=c('green','blue', "yellow", "red")) +
        labs(title = "No of Contact Trace over time (Random Data)")
    )
  })
  
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    # colorBy <- input$color
    # sizeBy <- input$size
    
    # if (colorBy == "superzip") {
    #   # Color and palette are treated specially in the "superzip" case, because
    #   # the values are categorical instead of continuous.
    #   colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
    #   pal <- colorFactor("viridis", colorData)
    # } else {
    #   colorData <- zipdata[[colorBy]]
    #   pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    # }
    # 
    # if (sizeBy == "superzip") {
    #   # Radius is treated specially in the "superzip" case.
    #   radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    # } else {
    #   radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    # }
    
    leafletProxy("map", data = health_condition()) %>%
      addResetMapButton() %>%
      addSearchOSM() %>%
      clearMarkers() %>%
      # clearShapes() %>%
      # clearGroup() %>% 
      
      leaflet::addCircleMarkers(
        data = health_condition(), 
        lng = ~longitude,
        lat = ~latitude,
        weight = 3, 
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
          health_condition()$condition == "green" ~ "#28a745",
          health_condition()$condition == "blue" ~ "#007bff", 
          health_condition()$condition == "yellow" ~ "#ffc107", 
          health_condition()$condition == "red" ~ "#dc3545"
        )
      )
    
  })
  
  ### reset session
  
  observeEvent(input$reset_button, {js$reset()}) 
  
  # Show a popup at the given location
  # showZipcodePopup <- function(zipcode, lat, lng) {
  #   selectedZip <- allzips[allzips$zipcode == zipcode,]
  #   content <- as.character(tagList(
  #     tags$h4("Score:", as.integer(selectedZip$centile)),
  #     tags$strong(HTML(sprintf("%s, %s %s",
  #                              selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
  #     ))), tags$br(),
  #     sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
  #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
  #     sprintf("Adult population: %s", selectedZip$adultpop)
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  # }
  
  # When map is clicked, show a popup with city info
  # observe({
  #   leafletProxy("map") 
  
  # %>% clearPopups()
  # event <- input$map_shape_click
  # if (is.null(event))
  #   return()
  # 
  # isolate({
  #   showZipcodePopup(event$id, event$lat, event$lng)
  # })
  # })
  
  
  ## Data Explorer ###########################################
  
  # observe({
  #   district <- input$district
  #     
  #   # stillSelected <- isolate(input$cities[input$cities %in% cities])
  #   # updateSelectizeInput(session, "cities", choices = cities,
  #   #                      selected = stillSelected, server = TRUE)
  # })
  
  # observe({
  #   zipcodes <- if (is.null(input$states)) character(0) else {
  #     cleantable %>%
  #       filter(State %in% input$states,
  #              is.null(input$cities) | City %in% input$cities) %>%
  #       `$`('Zipcode') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
  #   updateSelectizeInput(session, "zipcodes", choices = zipcodes,
  #                        selected = stillSelected, server = TRUE)
  # })
  # 
  # observe({
  #   if (is.null(input$goto))
  #     return()
  #   isolate({
  #     map <- leafletProxy("map")
  #     # map %>% clearPopups()
  #     dist <- 0.5
  #     lat <- input$goto$lat
  #     lng <- input$goto$lng
  #     showZipcodePopup(zip, lat, lng)
  #     map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  #   })
  # })
  
  output$covid_table <- DT::renderDataTable({
    df <- local_data 
    # %>%
    #   filter(
    #     Score >= input$minScore,
    #     Score <= input$maxScore,
    #     is.null(input$states) | State %in% input$states,
    #     is.null(input$cities) | City %in% input$cities,
    #     is.null(input$zipcodes) | Zipcode %in% input$zipcodes
    #   ) %>%
    #   mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '">
    #                         <i class="fa fa-crosshairs"></i></a>', sep=""))
    # action <- DT::dataTableAjax(session, df, outputId = "ziptable")
    
    DT::datatable(df, 
                  options=list(columnDefs = list(list(visible=FALSE, targets=c(5, 6, 7, 8)))),
                  # options = list(ajax = list(url = action)),
                  escape = FALSE)
  })
}