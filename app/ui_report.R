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
#if (sys.nframe() == 0L) { # if __name__ == '__main__'
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
    
    env_anaylsis <- list2env(
      list(
        keys = c("TPR", "TNR", "ACC", 'LR+', 'LR-', "PPV", "NPV", "FNR", "FPR", "FDR", "FOR", "F1", "MCC", "CK"),
        valid_ratios = c("TPR", "TNR", "PPV", "NPV", "FNR", "FPR", "FDR", "FOR"),
        valid_percentage = c("ACC"),
        keys_default = keys_default <- c("TPR", "TNR", "ACC", 'LR+', 'LR-', "PPV", "NPV")
      ),
      parent = emptyenv()
    )
    env_anaylsis$labels <- mapping_abbrv2label[env_anaylsis$keys] # unlist(mapping_abbrv2label[keys], use.names = FALSE)
    env_anaylsis$tips <- mapping_abbrv2tips[env_anaylsis$keys]
    
    # emulate reactive bindings
    data <- reactiveValues()
    analysis <- reactiveValues(
      selections_performance = env_anaylsis$keys_default,
      conf.level = 0.95,
      metrics = NULL,
      performance_metrics = NULL,
      ratios = NULL
    )
    
    selections <- reactiveValues(
      reference_file="reference_file",
      test_file="test_file",
      df_reference=NULL,
      df_test=NULL,
      column_reference="reference",
      column_test="test",
      choices_reference=c("positive", "negative"),
      choices_test=c("positive", "negative"),
      selected_reference_positive="positive",
      selected_test_positive="negative"
    )
    
    selections_performance <- keys_default
    makeReactiveBinding("selections_performance")
    
    observeEvent(input$btn_createReactive, {
      print("Create reactive variables")
      df <- suppressMessages(readr::read_csv("qualitative_both_methods.csv"))
      df <- convert_df_to_boolean(df[, c('reference', 'test')])
      
      selections$df_reference <- df
      selections$df_test <- df
      
      data$reference <- df$reference
      data$test <- df$test
  
      analysis$metrics <- calculate_metrics(data$reference, data$test)
      analysis$performance_metrics <- do.call(calculate_performance_metrics, analysis$metrics)
      analysis$confidence_intervals <- do.call(calculate_confidence_intervals, 
                                               c(analysis$metrics, analysis$conf.level))
      
      r <- add_additional_metrics(pm = analysis$performance_metrics,
                                  CI = analysis$confidence_intervals,
                                  df = data.frame(reference = data$reference,
                                                  test = data$test),
                                  conf.level = analysis$conf.level)
      analysis$performance_metrics <- r$pm
      analysis$confidence_intervals <- r$CI
    })
    
    # Testing -------------------------------------------------------------------
    
    report <- reactive({
      create_report_list(analysis, selections, env_anaylsis$keys)
    })
    
    reportServer(
      "report_full",
      outputfile = "Report_full.docx",
      file_template = "resources/templateReport.docx",
      report = report()
    )
    
    report_selected <- reactive({
      create_report_list(analysis, selections, selections_performance)
    })
    
    reportServer(
      "report_selected",
      outputfile = "Report.docx",
      file_template = "resources/templateReport.docx",
      report = report_selected()
    )
  
    
  }
  
  shinyApp(ui, server)
#}

