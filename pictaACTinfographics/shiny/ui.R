library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "PICTA ACT"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Single Image", tabName = "single_image", icon = icon("dice-one")),
      menuItem("Batch File", tabName = "batch_file", icon = icon("table")),
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
                  width = 4,
                  downloadButton('download_single', label = "Download PDF")
                )
              ),
              fluidRow(
                box(width = 6,
                    imageOutput("plot", width = "100%")
                ),
                box(width = 6,
                  verbatimTextOutput("asthma_statements")
                )
              )
      ),
      
      tabItem(tabName = "batch_file",
              fluidRow(
                box(
                  width = 4,
                  fileInput("file", label = h3("Batch file input")),
                  actionButton("action", label = "Generate batch files")
                ),
                box(
                  width = 8,
                  verbatimTextOutput("batch_file_pth"),
                )
              ),
              fluidRow(
                column(12,
                       DT::DTOutput('table')
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "debug",
              fluidRow(
                box(title = "PT Info",
                    verbatimTextOutput("pt_list")
                ),
                box(title = "R Session Info",
                    verbatimTextOutput("cwd"),
                    verbatimTextOutput("tinytex_info"),
                    verbatimTextOutput("plot_pth_debug"),
                    verbatimTextOutput("pdf_single_fn")
                )
              )
      )
    )
  )
)