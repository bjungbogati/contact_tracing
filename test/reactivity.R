library(dplyr)
library(shiny)
library(leaflet)


gender <- c("Male", "Female","Female", "Male")
Location <- c("AB", "BC", "CD", "DE")
hasTV <- c("Yes","Yes","No","No")
Latitude <- c(49.82380908513249, 59.478568831926395, 59.478568831926395, 49.82380908513249)
Longitude <- c(-10.8544921875, -10.8544921875, 2.021484375, 2.021484375)
DF <- data.frame(gender, Location, hasTV, Latitude, Longitude)

sex <- unique(DF$gender)
loc <- unique(DF$Location)
hastv <- unique(DF$hasTV)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  radioButtons("radio_hastv",label = "Has TV", choices = hastv, selected = "Yes"),
  checkboxGroupInput("checkbox_gender", label = "Gender", choices = sex, selected = sex),
  selectInput("Location", label = "Location", choices=loc, selected = "AB"),
  leafletOutput("map", height = 415)

)



server <- function(input, output, session) {

  
  filtered_data <- reactive({
    DF %>%
      filter(gender %in% input$checkbox_gender,
             hasTV %in% input$radio_hastv,
             Location %in% input$loc
      )
  })
  
  observe( {
    
    leafletProxy("mymap", data = filtered_data()) %>%
            clearMarkers()%>%
            addMarkers(radius =3)%>%
            label = ~htmlEscape(hasTV) # note change; using DF here is wrong
          }
  )
  
}

shinyApp(ui, server)