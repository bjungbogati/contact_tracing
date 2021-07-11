library(jsonlite)
library(shiny)
mydf <- data.frame("Data"=rnorm(12), 
                   "Months"=c("Jan", "Nov", "Dec", "Feb", 
                              "Mar", "Apr", "May", "Jun", 
                              "Jul", "Aug", "Sep", "Oct"), stringsAsFactors = FALSE)
ui <- fluidPage(
  # Input() function
  radioButtons(inputId = "myDateInterval", label = "Select Date Interval",
               choiceNames = list("Monthly","Quarterly","Annual"),
               choiceValues = list(toJSON(mydf$Month),
                                   toJSON(mydf$Month[seq(1,length(unique(mydf$Month)),3)]),
                                   toJSON(mydf$Month[1]))),
  
  # Output() functions
  tableOutput("results"))
# set up server object
server <- function(input, output) {
  output$results <-  renderTable({
    ipt = fromJSON(input$myDateInterval)
    ret = mydf[mydf$Months %in% ipt,]
    ret
  })
}
shinyApp(ui = ui, server = server)