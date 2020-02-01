library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    menuItem("Image", tabName = "image", icon = icon("dashboard"), selected = TRUE),
    menuItem("Debug", tabName = "debug", icon = icon("th"))
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "image",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(
                  width = 4,
                  selectInput("language", label = h3("Language"), 
                              choices = list("English" = "english", "Spanish" = "spanish"), 
                              selected = "english"),
                  textInput("Name", label = h3("Name"), value = "Enter name ...")
                ),
                
                box(
                  width = 4,
                  dateInput("today_date", label = h3("Today's date"), value = Sys.Date()),
                  numericInput("today_act", label = h3("Today ACT"), value = 25)
                ),
                box(
                  width = 4,
                  dateInput("previous_date", label = h3("Previous date"), value = Sys.Date()),
                  numericInput("previous_act", label = h3("Previous ACT"), value = 5),
                )
              ),
              fluidRow(
                imageOutput("plot")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "debug",
              h2("Widgets tab content"),
              fluidRow(
                box(width = 10,
                  verbatimTextOutput("pt_list")
                )
              )
      )
    )
  )
)
