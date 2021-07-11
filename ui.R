# Choices for drop-downs
vars <- c(
  "Green" = "green",
  "Blue" = "blue",
  "Yellow" = "yellow",
  "Red" = "red"
)


valueBox <- function(value, subtitle, icon, color) {
  div(class = "col-lg-2 col-md-6",
      div(class = "panel panel-primary", style = paste0("border-color:", color),
          div(class = "panel-heading", style = paste0("background-color:", color, ";", "border-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-2x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 26px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(style = ("font-size: 12px; font-weight: bold;"), 
                          div(subtitle)
                      ),
                  )
              )
          ),
          # 
          # div(class = "panel-footer",
          #     div(class = "clearfix")
          # )
      )
  )
}

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

header <- dashboardHeader(
  title = "Covid19 - Contact Tracing",
  tags$li(class = "dropdown", actionButton("signout", "Sign out", icon = icon("sign-out-alt")))
) |>
reqSignin()

title <- "Contact Tracing"

sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(
    # reqSignin(),

    menuItem("Contact Map", tabName = "contactmap", icon = icon("map")),
    menuItem("Data Explorer", tabName = "dataexplorer", icon = icon("table"))
  )
)



body2 <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  tabItems(
    tabItem(
      tabName = "contactmap",
      
    column(
      width = 12,
      height = "600px",
      
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width = "100%", height = 600)
      
      
      
      
      
    ),

   
      

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        br(),
        tags$h2("Contract Tracing", style = "display:inline;"),
        tags$a(
          href = "javascript:history.go(0)",
          popify(tags$i(class = "fa fa-refresh fa-2x"),
            title = "Reload",
            content = "Click here to restart the Shiny session",
            placement = "right"
          )
        ),
        br(), br(),
        checkboxGroupInput("condition", "Conditions",
          choices = vars, inline = F,
          selected = vars
        ),
        dateRangeInput(
          inputId = "date", "Date",
          format = "mm/dd/yy",
          separator = " - "
        ),
        sliderInput("timestamp", "Timeline:",
          min = min(local_data$date),
          max = max(local_data$date),
          value = range(local_data$date),
          step = 1,
          sep = ",",
          animate = TRUE
        ),
        plotOutput("histCentile", height = 200),
        plotOutput("scatterCollegeIncome", height = 250),
        # Include shinyjs in the UI
      )
      
    ),
    tabItem(
      tabName = "dataexplorer",
      fluidRow(
        valueBox(
          color = "black",
          value = "obs_row",
          icon = "user",
          subtitle = "Total Number of People"
        ),
        valueBox(
          color = "#28a745",
          value = "obs_green",
          icon = "user",
          subtitle = "People with no Symptoms"
        ),
        valueBox(
          color = "#007bff",
          value = "obs_blue",
          icon = "user",
          subtitle = "No symptoms with Travel History"
        ),
        valueBox(
          color = "#ffc107",
          value = "obs_yellow",
          icon = "user",
          subtitle = "Fever and Cough"
        ),
        valueBox(
          color = "#dc3545",
          value = "obs_red",
          icon = "user",
          subtitle = "Shortness of Breath, Fever and Cough"
        )
      ),
      hr(),
      plotlyOutput("contact_trace_plot"),
      DT::dataTableOutput("covid_table")
    )
  )
)



login_page <- div(
  id = "loginpage",
  icon("poll", class = "icons"),
  tags$h2("Covid19 Tracing", class = "text-center stitle"), br(),
  textInput("email_signin",
    placeholder = "Your email",
    label = tagList(icon("user"), "Email")
  ),
  passwordInput("password_signin",
    placeholder = "Your password",
    label = tagList(icon("unlock-alt"), "Password")
  ),
  br(),
  div(
    style = "text-align: center;",
    actionButton("signin", "SIGN IN", icon = icon("sign-in-alt")),
    shinyjs::hidden(
      div(
        id = "nomatch",
        tags$p("Oops! Incorrect username or password!",
          style = "color: red; font-weight: 600;padding-top: 5px;font-size:16px;",
          class = "text-center"
        )
      )
    ),
    br(), br(), br(),
    tags$h6("Powered by ", icon("r-project"), "Shiny | © Numeric Mind") # ,
    # tags$code("Username: myuser1  Password: mypass1")
  )
)


ui <- fluidPage(
  useFirebase(),
  reqSignout(login_page),
  dashboardPage(
    title = title, header = header,
    sidebar = reqSignin(sidebar),
    body = reqSignin(body2), skin = "blue"
  )
)
