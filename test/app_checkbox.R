library(shiny)
library(shinydashboard)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title="Membership Satisfaction"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics Dashboard", tabName = "demos", icon = 
                 icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "demos",
              sidebarPanel(
                checkboxGroupInput("inpt","Select variables to plot", choices = 
                                     c("Web" = 1,"Huddle" = 3, "Other" = 5, "Test" = 7)),
                checkboxGroupInput("role", 
                                   "Select Primary Role of Interest", 
                                   choices = c("Student" = 1, "Not" = 2)),
                checkboxGroupInput("range", 
                                   "Select year(S) of Interest", 
                                   choices = c("2016"=2,"July 2017"=1))),
              fluidPage(
                
                plotOutput("plot")
                
              )))))


server <- function(input,output){
  
  library(tidyverse)
  
  GapAnalysis_LongFormImpt <- structure(list(status = c(1, 5, 5, 1, 1, 5), year = c(1, 1, 1, 
                                                                                    1, 1, 1), Product = structure(c(2L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", 
                                                                                                                                                        "2", "3", "4"), class = "factor"), Score = c(1, 1, 3, 2, 2, 1
                                                                                                                                                        )), .Names = c("status", "year", "Product", "Score"), row.names = c(NA, 
                                                                                                                                                                                                                            6L), class = "data.frame")
  
  
  GapAnalysis_LongFormSat <- structure(list(status = c(5, 5, 1, 1, 5, 1), year = c(1, 1, 1, 
                                                                                   1, 1, 1), Product = structure(c(1L, 1L, 1L, 1L, 1L, 1L), .Label = c("1", 
                                                                                                                                                       "2", "3", "4"), class = "factor"), Score = c(2, 3, 2, 1, 1, 1
                                                                                                                                                       )), .Names = c("status", "year", "Product", "Score"), row.names = c(NA, 
                                                                                                                                                                                                                           6L), class = "data.frame")
  
x <- reactive({
    inpt <- as.double(input$inpt)
    role <- as.double(input$role)
    range <- as.double(input$range)
    
    GapAnalysis_LongFormSat %>%
      filter(Product %in% inpt,
             status %in% role,
             year %in% range) %>%
      summarize(Avg = mean(Score, na.rm = TRUE)) %>%
      pull(-1)
  })
  
  
  y <- reactive({
    inpt <- as.double(input$inpt)
    role <- as.double(input$role)
    range <- as.double(input$range)
    
    GapAnalysis_LongFormImpt %>%
      filter(Product %in% inpt,
             status %in% role,
             year %in% range) %>% 
      summarize(Avg = mean(Score, na.rm = TRUE))%>%
      pull(-1)
  })
  
  xyCoords<- reactive({
    x <- x()
    y <- y()
    
    data.frame(col1=x, col2=y)})
  
  output$plot <- renderPlot({
    xyCoords <- xyCoords()
    
    xyCoords %>% 
      ggplot(aes(x = col1, y = col2)) +
      geom_point(colour ="green", shape = 17, size = 5 )+
      labs(x = "Mean Satisfaction", y = "Mean Importance") +
      xlim(0,5) + ylim(0,5) +
      geom_vline(xintercept=2.5) + 
      geom_hline(yintercept =  2.5)})
  
}


shinyApp(ui = ui, server = server)