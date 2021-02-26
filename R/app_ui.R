#' main app ui
#'
#' @return shiny ui
#' @export
#'

app_ui <- function(){
  exams=sort(unique(hotakapackage::AA$Exam))
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title="Score Relationships between Two Exams",
                                    titleWidth = 700
    ),
    # dashboardSidebar(disable = TRUE),
    # dashboardBody(
    # fluidRow(
    #   box(
    # sidebarLayout(

    shinydashboard::dashboardSidebar(
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
      shiny::sliderInput("graddate",
                         "Expected Graduation Year Range",
                         min = 1900,
                         max = 2100,
                         value = c(1900,2100),
                         step=1),
      shiny::checkboxInput("LimitN",
                           "Limit N to 1,000 (for faster load time)",
                           value=T),
      shiny::checkboxInput("Jitter",
                           "Jitter (spread cases for visual clarity)",
                           value=T),
      shiny::checkboxInput("Outliers",
                           "Remove Outliers (for visual clarity)",
                           value=F)),
    # ),
    shinydashboard::dashboardBody(
      shiny::fluidRow(shinydashboard::box(shiny::plotOutput("plot1"),height = 650,width = 700),
                      shinydashboard::box(shiny::textOutput("note"),height=100,width = 700)
      )
    )
  )
}
