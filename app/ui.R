library(shiny)
require(shinyjs)
require(shinyWidgets)


source('ui_report.R')
source('ui_analysis.R')

labelWithTooltip <- function(text, ..., tooltip = NULL, required = FALSE) {
  text <- paste0(text, paste0(...))
  if (is.null(tooltip)) {
    if (required) {
      tags$div(text, tags$span('*', style='color: #f00;'))
    } else {
      text
    }
  } else {
    if (required) {
      tags$div(text, tags$span('*', style='color: #f00;'), title=tooltip)
    } else {
      tags$div(text, title=tooltip)
    }
  }
  
}

ui = fluidPage(
  
  # used : https://stackoverflow.com/questions/31703241/activate-tabpanel-from-another-tabpanel/31719425#31719425
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(script = "utils.js",
                         functions = c('disableTab','enableTab')),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  titlePanel("Test Method Evaluator"),
  
  navbarPage(NULL, id='sidebar',
               #widths = c(2, 10),
               selected = "Input",
    
  # section: Input  ----------------------------------------------------------
  
  tabPanel("Input", wellPanel(
    h1("Input"),
    
    sidebarLayout(
      # section: Input : sidebarPanel ----------------------------------------
      sidebarPanel(
        
        fluidRow(
          fileInput("file_reference",
                    labelWithTooltip("Upload reference file (CSV)", required = TRUE, 
                                     tooltip = paste0(
                                       "File with the values of the reference methode. ",
                                       "Requires at least one column with binary values ",
                                       "(optional: multiple columns for reference and test)")), 
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),
        ), # fluidRow
        
        conditionalPanel(
          condition = 'output.file_reference_uploaded',
          fluidRow(
            p('Optional'),
            fileInput("file_test", 
                      labelWithTooltip("Upload test file (CSV)",
                                       tooltip = paste0(
                                         "File with the values of the test methode. ",
                                         "requires at least one column with binary values")
                                        ),
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")),
          ) # fluidRow
        ), # conditionalPanel
        
        conditionalPanel(
          condition = '!output.upload_valid',
          htmlOutput("text_input_message", style='color : red;')
        ), # conditionalPanel
        conditionalPanel(
          condition = 'output.upload_valid',
          actionButton("btn_analyze", "Run Analysis", style="background-color:#26A65B", width="100%")
        ) # conditionalPanel
      ), # sidebarPanel
      
      # section: Input :mainPanel --------------------------------------------
      mainPanel(
        # Options Reference File ----------------------------------------------
        conditionalPanel(
          condition = 'output.file_reference_uploaded',
          wellPanel(
            fluidRow(
              h4('Reference method'),
              column(6,
                selectInput("selected_reference_file", 
                            labelWithTooltip("Select file", tooltip = "Select the file that should be used.", required = TRUE), 
                            choices=c()),
                selectInput("selected_reference_column", 
                            labelWithTooltip("Select column", tooltip = "Select the column with binary values.", required = TRUE),
                            choices=c())
              ), # column
              column(6,
                     conditionalPanel(
                       condition = 'output.reference_column_valid',
                       fluidRow(
                         align='top',
                         column(6,
                                selectInput("selected_reference_positive", 
                                            labelWithTooltip("positive", tooltip = "Select the value for positive results", required = TRUE), 
                                            choices = c()),
                         ),
                         column(6,
                                tags$b("negative", title="automatically set"),
                                verbatimTextOutput('selected_reference_negative', placeholder=TRUE)
                         )
                       ) # fluid row
                     ), # conditionalPanel
                     conditionalPanel(
                       condition = '!output.reference_column_valid',
                       span(HTML("This is not a valid column.<br> Only <b>2</b> categories are allowed."), style='color:red')
                     )
              )
            ),# fluidRow
          ) # wellPanel
        ), # conditionalPanel
        
        # Options Test File ---------------------------------------------------
        conditionalPanel(
          condition = 'output.file_reference_uploaded',
          wellPanel(
            fluidRow(
              h4('Test method', noWS=TRUE),
              column(6,
                selectInput("selected_test_file", 
                            labelWithTooltip("Select file", tooltip = "Select the file that should be used.", required = TRUE), 
                            choices=c()),
                selectInput("selected_test_column", 
                            labelWithTooltip("Select column", tooltip = "Select the column with binary values.", required = TRUE), 
                            choices = c())
              ), # column
              column(6,
                     conditionalPanel(
                       condition = 'output.test_column_valid',
                       fluidRow(
                         align='top',
                         column(6,
                                selectInput("selected_test_positive", 
                                            labelWithTooltip("positive", tooltip = "Select the value for positive results", required = TRUE),
                                            choices = c()),
                         ),
                         column(6,
                                tags$b("negative", title="automatically set"),
                                verbatimTextOutput('selected_test_negative', placeholder=TRUE)
                         )
                       ) # fluid row
                     ), # conditionalPanel
                     conditionalPanel(
                       condition = '!output.test_column_valid',
                       span(HTML("This is not a valid column.<br> Only <b>2</b> categories are allowed."), style='color:red')
                     )
              )
            ),# fluidRow
          ) # wellPanel
        ), # conditionalPanel
        
      ) #mainPanel
    ) # sidebarLayout
  )), # tabPanel - section
  
  # section: Analysis ---------------------------------------------------------
  tabPanel("Analysis", wellPanel(
     h1("Analysis"),
     fluidRow(
       column(4, 
              h4("2x2 contingency table"),
              tableOutput('confusion_matrix' ),
              verbatimTextOutput("text", placeholder = TRUE)),
       performanceUi("performance")
     ) # fluidRow
  )), # tabPanel
  
  # section: Report ---------------------------------------------------------
  
  tabPanel("Report", wellPanel(
     fluidRow(
       reportUi("report_selected",
                label="Download",
                text="Download the report for the selected metrics."
                ),
       hr(),
       reportUi("report_full",
                label="Download Full Report",
                text="Download the full report (DOCX)."
                ),
     ) #fluidRow
  )) # tabPanel
  
  ) # navlistPanel
)