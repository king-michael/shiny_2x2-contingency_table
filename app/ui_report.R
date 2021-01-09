library(shiny)
library(MazamaCoreUtils)
logger.setup(infoLog="server.log")

source("report.R", local = FALSE)


reportUi <- function(id, label="Download", text="Download the full report a DOCX file.") {
  ns <- NS(id)
  
  tagList(
    p(text),
    # Button
    downloadButton(ns("report"), label)
  ) # tagList
} # reportUi
  
reportServer <- function(id, outputfile, file_template, report) {
  moduleServer(
    id,
    function(input, output, session) {
      output$report <- downloadHandler(
        filename = outputfile,
        content = function(file) {
          logger.info(sprintf('downloadHandler("%s") : create report file: "%s" from "%s" for user: "%s"', 
                              id, outputfile, file_template, report$username))
          generate_report_docx(file, file_template, report)
        }
      )
    }
  ) # moduleServer
} # reportServer



  
# Test ------------------------------------------------------------------------
if (sys.nframe() == 0L) { # if __name__ == '__main__'
  print("Run TEST VERSION")
  
  source('statistics.R')
  source('utils.R')
  source('localization.R')
  
  ui <- fluidPage(
    actionButton("btn_createReactive", "DEBUG: create reactive Input"),
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
  )
  
  server <- function(input, output, session) {
    # PREPROCESSING -------------------------------------------------------------
    # set the defaults 
    localization <- Localization$new("localization.xml")
    mapping_abbrv2label <- localization$get_map_attr2attr_from_xpath('metrics/metric', 'abbrv', 'label')
    mapping_abbrv2tips <- localization$get_map_attr2text_from_xpath('metrics/metric', 'abbrv')
    
    keys <- c("TPR", "TNR", "PPV", "NPV", "FNR", "FPR", "FDR", "FOR", "ACC", "F1", "MCC")
    keys_default <- c("TPR", "TNR", "PPV", "ACC")
    
    labels <- mapping_abbrv2label[keys] # unlist(mapping_abbrv2label[keys], use.names = FALSE)
    tips <- mapping_abbrv2tips[keys]
    
    # emulate reactive bindings
    data <- reactiveValues()
    analysis <- reactiveValues()
    
    selections_performance <- keys_default
    makeReactiveBinding("selections_performance")
    
    observeEvent(input$btn_createReactive, {
      print("Create reactive variables")
      df <- suppressMessages(readr::read_csv("qualitative_both_methods.csv"))
      df <- convert_df_to_boolean(df[, c('reference', 'test')])
      
      data$reference <- df$reference
      data$test <- df$test
  
      analysis$metrics <- calculate_metrics(data$reference, data$test)
      analysis$performance_metrics <- do.call(calculate_performance_metrics, analysis$metrics)
    })
    
    # Testing -------------------------------------------------------------------
    
    report <- reactive({
      create_report_list(analysis$metrics, analysis$performance_metrics, keys)
    })
    
    reportServer(
      "report_full",
      outputfile = "Report_full.docx",
      file_template = "resources/templateReport.docx",
      report = report()
    )
    
    report_selected <- reactive({
      create_report_list(analysis$metrics, analysis$performance_metrics, selections_performance)
    })
    
    reportServer(
      "report_selected",
      outputfile = "Report.docx",
      file_template = "resources/templateReport.docx",
      report = report_selected()
    )
  
    
  }
  
  shinyApp(ui, server)
}
