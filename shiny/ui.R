library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "PICTA ACT"),
  dashboardSidebar(disable = TRUE
  ),
  dashboardBody(
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
      box(height = "850px",
          width = 12,
          imageOutput("plot")
      )
      
    ),
    fluidRow(
      box(collapsed = TRUE,
          collapsible = TRUE,
          title = "Debug Info",
          verbatimTextOutput("pt_list")
      )
      
    )
  )
)
