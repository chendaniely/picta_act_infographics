library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "PICTA ACT"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Single Image", tabName = "single_image", icon = icon("dice-one")),
      #menuItem("Batch File", tabName = "batch_file", icon = icon("table")),
      menuItem("Debug", tabName = "debug", icon = icon("bug"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "single_image",
              fluidRow(
                box(
                  width = 4,
                  selectInput("language", label = h3("Language"), 
                              choices = list("English" = "english", "Spanish" = "spanish"), 
                              selected = "english"),
                  textInput("name", label = h3("Name"), value = "", placeholder = "Enter name ...")
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
                box(
                  width = 3,
                  downloadButton('download_single', label = "Download PDF")
                )
              ),
              fluidRow(
                box(
                  verbatimTextOutput("asthma_statements")
                )
              ),
              fluidRow(
                box(#height = "100%",
                  width = 12,
                  imageOutput("plot", width = "100%")
                )
                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "debug",
              fluidRow(
                box(title = "Debug Info",
                    verbatimTextOutput("pt_list")
                ),
                box(
                  verbatimTextOutput("cwd"),
                  verbatimTextOutput("plot_pth_debug")
                )
              )
      )
    )
  )
)