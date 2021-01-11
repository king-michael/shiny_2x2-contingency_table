require(shiny)
require(shinyjs)
require(shinyWidgets)

require(dplyr)
library(MazamaCoreUtils)
logger.setup(infoLog="server.log")

source('utils.R')
source('statistics.R')
source('localization.R')
source('ui_report.R')
source('ui_analysis.R')


server = function(input, output, session) {
  
  localization <- Localization$new("localization.xml")
  mapping_abbrv2label <- localization$get_map_attr2attr_from_xpath('metrics/metric', 'abbrv', 'label')
  mapping_abbrv2tips <- localization$get_map_attr2text_from_xpath('metrics/metric', 'abbrv', strip = TRUE)
  
  
  # disable tab2 on page load
  js$disableTab("Analysis")
  js$disableTab("Report")
  
  doAnalysis <- FALSE
  makeReactiveBinding("doAnalysis")
  
  uploaded_files <- reactiveValues(
    reference=list(
      df=NULL,
      filename=NULL
    ),
    test=list(
      df=NULL,
      filename=NULL
    )
  )
  
  selections <- reactiveValues(
    reference_file=NULL,
    test_file=NULL,
    df_reference=NULL,
    df_test=NULL,
    column_reference=NULL,
    column_test=NULL,
    choices_reference=NULL,
    choices_test=NULL
  )
  
  
  
  # section: Upload : Upload files --------------------------------------------
  
  observeEvent(input$file_reference, {
    inFile <- input$file_reference
    df <- readr::read_csv(inFile$datapath)
    logger.info(sprintf('Upload reference file (name: "%s"; size : %dB)', inFile$name, inFile$size))
    
    uploaded_files$reference <- list(
      df=df,
      filename=inFile$name
    )
    
  })
  
  observeEvent(input$file_test, {
    inFile <- input$file_test
    df <- readr::read_csv(inFile$datapath)
    logger.info(sprintf('Upload reference file (name: "%s"; size : %dB)', inFile$name, inFile$size))
    
    uploaded_files$test <- list(
      df=df,
      filename=inFile$name
    )
  })
  
  # section: Upload : Options : Update SelectInput of file selections ---------
  
  output$file_reference_uploaded <- reactive({
    # set choices for reference file
    choices <- c(uploaded_files$reference$filename, uploaded_files$test$filename)
    if (length(choices) == 0) {return(FALSE)}
    updateSelectInput(session, "selected_reference_file",
                      choices = choices, selected = choices[1])
    
    # set default for test file
    if (length(choices) == 1 && is.null(uploaded_files$test$filename)) {
      updateSelectInput(session, "selected_test_file",
                        choices = choices, selected = tail(choices, n=1))
    }
    
    return(TRUE)
  })
  outputOptions(output, 'file_reference_uploaded', suspendWhenHidden=FALSE)
  
  output$file_test_uploaded <- reactive({
    # set choices for test file
    choices <- c(uploaded_files$reference$filename, uploaded_files$test$filename)
    if (length(choices) == 0) {return(FALSE)}
    updateSelectInput(session, "selected_test_file",
                      choices = choices, selected = tail(choices, n=1))
    return(TRUE)
  })
  outputOptions(output, 'file_test_uploaded', suspendWhenHidden=FALSE)
  
  # section: Upload : Options : Update SelectInput of column selections -------
  
  observeEvent(input$selected_reference_file, {
    if (nchar(input$selected_reference_file) == 0) {return()}
    selections$reference_file <- input$selected_reference_file
    choices <- c(uploaded_files$reference$filename, uploaded_files$test$filename)
    index <- which(choices %in% input$selected_reference_file)
    dataset <- list(uploaded_files$reference, uploaded_files$test)[[index]]
    
    columns <- colnames(dataset$df)
    updateSelectInput(session, "selected_reference_column", 
                      choices = columns, selected = columns[1])
    selections$df_reference <- dataset$df
  })
  
  observeEvent(input$selected_test_file, {
    if (nchar(input$selected_test_file) == 0) {return()}
    selections$test_file <- input$selected_test_file
    choices <- c(uploaded_files$reference$filename, uploaded_files$test$filename)
    index <- which(choices %in% input$selected_test_file)
    dataset <- list(uploaded_files$reference, uploaded_files$test)[[index]]
    
    columns <- colnames(dataset$df)
    updateSelectInput(session, "selected_test_column", 
                      choices = columns, selected = columns[1])
    selections$df_test <- dataset$df
  }) # observe
  
  reference_column_valid <- reactive({
    column <- input$selected_reference_column
    if (nchar(column) == 0) { return(FALSE) }
    values <- selections$df_reference[[column]]
    
    # short sample to speed up big datasets
    if (length(values) > 1000) {
      n_unique <- length(unique(values[1:10])) 
      if (n_unique > 2) {return(FALSE)}
    }
    
    n_unique <- length(unique(values))
    if (n_unique > 2) {
      return(FALSE)
    } else {
      choices <- unique(values)
      updateSelectInput(session, "selected_reference_positive", 
                        choices = choices, selected = choices[1])
      
      selections$column_reference <- column
      selections$choices_reference <- choices
    }
  })
  output$reference_column_valid <- reactive(reference_column_valid())
  outputOptions(output, 'reference_column_valid', suspendWhenHidden=FALSE)
  
  test_column_valid <- reactive({
    column <- input$selected_test_column
    if (nchar(column) == 0) { return(FALSE) }
    
    values <- selections$df_reference[[column]]
    if (is_valid_column(values)) {
      choices <- unique(values)
      updateSelectInput(session, "selected_test_positive", 
                        choices = choices, selected = choices[1])
      
      selections$column_test <- column
      selections$choices_test <- choices
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  output$test_column_valid <- reactive(test_column_valid())
  outputOptions(output, 'test_column_valid', suspendWhenHidden=FALSE)
  
  
  observeEvent(input$selected_reference_positive, {
    inp <- input$selected_reference_positive
    if (nchar(inp) == 0) { return(FALSE) }
    
    choices <- selections$choices_reference
    if (length(choices) == 2) {
      negative <- choices[(choices != inp)]
    } else {
      negative <- "NO negative value found"
    }
    output$selected_reference_negative <- renderText(negative)
  })
  
  observeEvent(input$selected_test_positive, {
    inp <- input$selected_test_positive
    if (nchar(inp) == 0) { return(FALSE) }
    
    choices <- selections$choices_test
    if (length(choices) == 2) {
      negative <- choices[(choices != inp)]
    } else {
      negative <- "NO negative value found"
    }
    output$selected_test_negative <- renderText(negative)
  })

  # section: Upload : Check valid output --------------------------------------
  data <- reactiveValues(
    reference=NULL,
    test=NULL
  )
  
  output$upload_valid <- reactive({
    if (!reference_column_valid() || !test_column_valid()) { return(FALSE) }
    
    if ((input$selected_reference_file == input$selected_test_file) &&
        (selections$column_reference ==selections$column_test)) { return(FALSE) }
    
    logger.info(sprintf('convert %s (column: "%s"; TRUE: %s, choices: [%s])',
                  input$selected_reference_file, selections$column_reference, 
                  input$selected_reference_positive, paste(selections$choices_reference, collapse=',')))
    df_ref <- convert_vector_to_boolean(selections$df_reference[[ selections$column_reference ]], 
                                            true_value=input$selected_reference_positive, 
                                            use_levels=selections$choices_reference)
    
    logger.info(sprintf('convert %s (column: "%s"; TRUE: %s, choices: [%s])',
                  input$selected_test_file, selections$column_test, 
                  input$selected_test_positive, paste(selections$choices_test, collapse=',')))
    df_test <- convert_vector_to_boolean(selections$df_test[[ selections$column_test ]], 
                                               true_value=input$selected_test_positive, 
                                               use_levels=selections$choices_test)
    
    
    if (length(df_test) != length(df_ref)) {
      # TODO: add user warning
      return(FALSE)
    }
    data$reference <- df_ref
    data$test <- df_test
    return(TRUE)
  })
  outputOptions(output, 'upload_valid', suspendWhenHidden=FALSE)
  
  observeEvent(input$btn_analyze, {
    print("run analysis")
    print(doAnalysis)
    doAnalysis <<- TRUE
    js$enableTab("Analysis")
    js$enableTab("Report")
    updateNavlistPanel(session, "sidebar", selected = "Analysis")
  })
  
  #END: Upload files ----------------------------------------- Upload section #
  # section: Analysis ---------------------------------------------------------
  
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
  
  # draw performance selections
  shinyWidgets::updateCheckboxGroupButtons(session, "performance-selected",
                                           choiceNames = unlist(env_anaylsis$labels, use.names = FALSE),
                                           choiceValues = env_anaylsis$keys,
                                           selected = env_anaylsis$keys_default,
                                           checkIcon = list(yes = icon("ok", lib = "glyphicon"))
  )
  
  # create stprage for analysis
  analysis <- reactiveValues(
    selections_performance = env_anaylsis$keys_default,
    conf.level = 0.95,
    metrics = NULL,
    performance_metrics = NULL,
    ratios = NULL
  )
  
  # calculate the values
  observeEvent(doAnalysis, {
    analysis$metrics <- calculate_metrics(data$reference, data$test)
    analysis$ratios <- do.call(get_ratios, analysis$metrics)
    
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
    
    # draw the confusion matrix
    output$confusion_matrix <- renderTable(as.confusionmatrix(analysis$metrics), rownames=TRUE)
    doAnalysis <<- FALSE
  }, ignoreInit = TRUE)
  
  # draw performance table
  observe({
    req(analysis$performance_metrics, analysis$ratios, cancelOutput = TRUE)
    analysis$selections_performance <<- input[["performance-selected"]]
    
    performanceServer("performance", 
                      analysis=analysis,
                      env=env_anaylsis)
    
  })
  
  # section: Report -----------------------------------------------------------
  
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