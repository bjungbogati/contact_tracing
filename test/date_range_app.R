library(shiny)
library(tidyverse)
library(lubridate)

# tweets <- read.csv(file.choose())

st <- ymd("2009-05-01")
en <- ymd("2018-02-28")
dates <- seq.Date(from = st, to = en, by = 1)
tweets <- tibble(date = dates, count = rnorm(length(dates), mean = 5, sd = 3))


ui <- fluidPage(
  dateRangeInput(inputId = "date",
                 strong("Date Range"),
                 start = "2009-05-04", end = "2018-02-28",
                 min = "2009-05-04", max ="2018-02-28" ),
  plotOutput("Graph")
)

server <- function(input, output) {
  
  newtweets <- reactive({
    filter(tweets, between(date, input$date[1], input$date[2]))
  })
  
  output$Graph <- renderPlot({
    ggplot(newtweets(), aes(x = date, y = count)) +
      geom_bar(stat = "identity", position = "stack") +
      #scale_y_continuous(name = "Retweet Count", limits = c(0,370000), breaks=seq(0,370000,10000)) +
      theme(panel.background = element_rect(fill = "white", colour = "grey50"))
  })
}

shinyApp(ui = ui, server = server)