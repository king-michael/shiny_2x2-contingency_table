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
  
  
  upload_error_message <- list(
    file_ref = "Please upload a CSV file with reference data. (Test data can be included as column or uploaded seperatly later.)",
    col_ref = "Please select a column for the reference data (2 categories).",
    col_test = "Please select a column for the test data (2 categories)."
  )

  valid_input_options <- reactiveValues(
    file_ref = FALSE,
    col_ref = FALSE,
    col_test = FALSE
  )
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
    choices_test=NULL,
    selected_reference_positive=NULL,
    selected_test_positive=NULL
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
    valid_input_options$file_ref <<- TRUE
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
      return(TRUE)
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
    
    selections$selected_reference_positive <- input$selected_reference_positive
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
    selections$selected_test_positive <- input$selected_test_positive
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
    doAnalysis <<- TRUE
    js$enableTab("Analysis")
    js$enableTab("Report")
    updateNavlistPanel(session, "sidebar", selected = "Analysis")
  })
  
  # change message field
  observe({
    text <- c(HTML("<b>Requirements:<br></b>"))
    if (!valid_input_options$file_ref) {
      text <- c(text, paste("-", upload_error_message$file_ref))
      output$text_input_message <- renderUI(HTML(text))
      return()
    }
    valid_input_options$col_ref <<- reference_column_valid()
    valid_input_options$col_test <<- test_column_valid()
    
    opts <- reactiveValuesToList(valid_input_options)
    if (any(opts ==FALSE)) {
      text <- c(text, paste("-", upload_error_message[names(which(opts == FALSE))], collapse = "<br>"))
      output$text_input_message <- renderUI(HTML(text))
    }
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
  
  observe({
    output$text<- renderPrint({
      req(data$reference, 
          analysis$performance_metrics,
          analysis$confidence_intervals,
          cancelOutput = TRUE)
      
      pm <- analysis$performance_metrics
      CI <- analysis$confidence_intervals
      if (is.null(CI$CK)) { return() }
      form_CI <- function(x) {sprintf("%s-%s", x[1], x[2])}
      
      n_samples <- length(data$reference)
      n_pos <- sum(data$reference)
      n_neg <- sum(!data$reference)
      n_pos_test <- sum(data$test)
      n_neg_test <- sum(!data$test)
      text <- form("Reference method
---------------------------------
N_samples : {n_samples} 
positive/negative : {n_pos}/{n_neg} ({p_pos}/{p_neg}%)
 
Test method
---------------------------------
N_samples : {n_samples} 
positive/negative : {n_pos_test}/{n_neg_test} ({p_pos_test}/{p_neg_test}%)
 
 
FDA required metrics (CI={conf.level}%)
---------------------------------
sensitivity : {TPR}% CI: {CI_TPR}%
specificity : {TNR}% CI: {CI_TNR}%
accuracy    : {ACC}% CI: {CI_ACC}%

LR+         : {LRp} CI: {CI_LRp}
LR-         : {LRn} CI: {CI_LRn}

Performance measure
---------------------------------
                                  (1 is the best, 0 is random)
Cohen's kappa (k)                : {CK} CI: {CI_CK} 
Matthews correlation coefficient : {MCC}
", n_samples=n_samples,
                   n_pos=n_pos, p_pos=round(n_pos/n_samples*100),
                   n_neg=n_neg, p_neg=round(n_neg/n_samples*100),
                   n_pos_test=n_pos_test, p_pos_test=round(n_pos_test/n_samples*100),
                   n_neg_test=n_neg_test, p_neg_test=round(n_neg_test/n_samples*100),
                   conf.level=round(100*analysis$conf.level),
                   TPR=sprintf("%6.2f", round(pm$TPR*100, 2)), CI_TPR=form_CI(round(CI$TPR*100, 2)),
                   TNR=sprintf("%6.2f", round(pm$TNR*100, 2)), CI_TNR=form_CI(round(CI$TNR*100, 2)),
                   ACC=sprintf("%6.2f", round(pm$ACC*100, 2)), CI_ACC=form_CI(round(CI$ACC*100, 2)),
                   LRp=sprintf("%6.2f", round(pm[['LR+']], 2)), CI_LRp=form_CI(round(CI[['LR-']], 2)),
                   LRn=sprintf("%6.2f", round(pm[['LR-']], 2)), CI_LRn=form_CI(round(CI[['LR-']], 2)),
                   CK=sprintf("%6.3f", round(pm$CK, 2)), CI_CK=form_CI(round(CI$CK, 3)),
                   MCC=sprintf("%6.3f", round(pm$MCC, 2))
      )
      
    })
  })


  # section: Report -----------------------------------------------------------
  
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
    create_report_list(analysis, selections, analysis$selections_performance)
  })
  
  reportServer(
    "report_selected",
    outputfile = "Report.docx",
    file_template = "resources/templateReport.docx",
    report = report_selected()
  )
  
  
}