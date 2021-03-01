#' main app ui
#'
#' @return shiny ui
#' @export
#'

app_ui <- function(){
  ShinyData=readRDS("//ord-isi-data/public/Staff_Folders/Hotaka_Maeda/RShiny/Data/ShinyData_0.rds")
  exams=sort(unique(ShinyData$Exam))
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title="NBOME Exam Score Analysis",
                                    titleWidth = 700
    ),

    shinydashboard::dashboardSidebar(
      width=300,
      sidebarMenu(
        shinydashboard::menuItem("Univariate View", tabName = "Univariate",badgeLabel = "click here", badgeColor = "red"),
        shinydashboard::menuItem("Bivariate View", tabName = "Bivariate",badgeLabel = "click here", badgeColor = "green")),
      shiny::selectInput("Exam1",
                         "Exam 1",
                         exams,
                         selected="FM"),
      shiny::dateRangeInput("Exam1date",
                            "Exam 1 Date Range",
                            start = "2000-01-01",
                            end = lubridate::today()),
      shiny::selectInput("Exam2",
                         "Exam 2",
                         exams,
                         selected="IM"),
      shiny::dateRangeInput("Exam2date",
                            "Exam 2 Date Range",
                            start = "2000-01-01",
                            end = lubridate::today()),
      shiny::uiOutput('choose_graddate'),
      shiny::checkboxInput("LimitN",
                           "Limit N to 500 (faster loading)",
                           value=T),
      shiny::checkboxInput("Jitter",
                           "Jitter (visual clarity)",
                           value=T),
      shiny::checkboxInput("Outliers",
                           "Remove Outliers (visual clarity)",
                           value=T)),
    # ),
    shinydashboard::dashboardBody( # the width should add to 12
      tags$head(
        tags$style(HTML(".main-sidebar { font-size: 17px; }")) #change the font size
      ),
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "Univariate",
          h2("Univariate Analysis (candidates that took at least 1 Exam)"),
          shiny::fluidRow(shinydashboard::box(shiny::tableOutput("tableU1"),height = 550,width=4),
                          shinydashboard::box(shiny::plotOutput("plotU1"),height = 550,width=4),
                          shinydashboard::box(shiny::plotOutput("plotU2"),height = 550,width=4),
          ),
          "Note. Only 1st time attempt data are included. Blue lines show the medians."
        ),
        shinydashboard::tabItem(
          tabName = "Bivariate",
          h2("Bivariate Analysis (candidates that took both Exams)"),
          shiny::fluidRow(shinydashboard::box(shiny::tableOutput("table1"),height = 550,width=4),
                          shinydashboard::box(shiny::plotOutput("plot1"),height = 550,width=8)
          ),
          "Note. Only 1st time attempt data are included. Blue lines show the medians."
          )
      )
    )
  )
}


