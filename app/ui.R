library(shiny)
require(shinyjs)


ui = fluidPage(
  
  # used : https://stackoverflow.com/questions/31703241/activate-tabpanel-from-another-tabpanel/31719425#31719425
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(script = "utils.js",
                         functions = c('disableTab','enableTab')),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  titlePanel("Evaluate Test Method"),
  
  navlistPanel("Workflow",
               widths = c(2, 10),
               selected = "Upload",
    
  
  # Upload section ------------------------------------------- Upload section #
  
  tabPanel("Upload",
    h1("Upload"),
    sidebarLayout(
      sidebarPanel(
        
        fluidRow(
          fileInput("file_reference", "Upload reference file (CSV)", 
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),
        ), # fluidRow
        
        conditionalPanel(
          condition = 'output.file_reference_uploaded',
          fluidRow(
            p('Optional'),
            fileInput("file_test", "Upload test file (CSV)", accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv")),
          ) # fluidRow
        ), # conditionalPanel
        
        conditionalPanel(
          condition = '!output.upload_valid',
          p("please select a column for the reference and for the test data" ,
            style='color : red; font-weight:bold')
        ), # conditionalPanel
        conditionalPanel(
          condition = 'output.upload_valid',
          actionButton("btn_analyze", "Run Analysis", style="background-color:#26A65B")
        ) # conditionalPanel
      ), # sidebarPanel
      
      mainPanel(
        # Options Reference File ----------------------------------------------
        conditionalPanel(
          condition = 'output.file_reference_uploaded',
          wellPanel(
            fluidRow(
              h4('Reference method'),
              column(6,
                selectInput("selected_reference_file", "Select file", c()),
                selectInput("selected_reference_column", "Select column", c())
              ), # column
              column(6,
                     conditionalPanel(
                       condition = 'output.reference_column_valid',
                       fluidRow(
                         align='top',
                         column(6,
                                selectInput("selected_reference_positive", "positive", c()),
                         ),
                         column(6,
                                tags$b("negative"),
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
                selectInput("selected_test_file", "Select file", c()),
                selectInput("selected_test_column", "Select column", c())
              ), # column
              column(6,
                     conditionalPanel(
                       condition = 'output.test_column_valid',
                       fluidRow(
                         align='top',
                         column(6,
                                selectInput("selected_test_positive", "positive", c()),
                         ),
                         column(6,
                                tags$b("negative"),
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
  ), # tabPanel - section
  
  # Analysis section --------------------------------------- Analysis section #

  tabPanel("Analysis",
    h1("Analysis"), 
    column(3,
           fileInput("file", "Choose CSV File", accept = ".csv")
    )
  ), # tabPanel
  
  # Report section ------------------------------------------- Report section #
  
  tabPanel("Report",
    p("Download the full report a DOCX file."),
    # Button
    downloadButton("downloadData", "Download")
  ) # tabPanel
  
  ) # navlistPanel
)