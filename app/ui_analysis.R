library(shiny)
require(DT)
require(shinyWidgets)
require(shinyjs)
library(MazamaCoreUtils)
logger.setup(infoLog="server.log")

source('utils.R')
source('statistics.R')
source('localization.R')


performanceUi <- function(id) {
  ns <- NS(id)
  
  column(8,
    fluidRow(column(4, h4("Performance measures"))),
    fluidRow(
      column(4, 
             DT::dataTableOutput(ns('table')),
             shinyWidgets::materialSwitch(
               inputId = ns("toggle_selection"),
               label = "Change Metrics", 
               value = FALSE,
               status = "info"
             ), # materialSwitch
      ), # column
      column(4,
             shinyWidgets::checkboxGroupButtons(
               inputId = ns("selected"),
               label = "Select performance metrics",
               choices = c("DEBUG"),
               selected = 1,
               justified = TRUE,
               direction = "vertical",
               checkIcon = list(yes = icon("ok", lib = "glyphicon"))
             ) # checkboxGroupButtons
      ) # column
    ) # fluidRow
  ) # tagList
} # reportUi


performanceServer <- function(id,
                              analysis,
                              env) {
    moduleServer(
      id,
      function(input, output, session) {
        valid_ratios <- env$valid_ratios
        valid_percentage <- env$valid_percentage
        labels <- env$labels
        tips <- env$tips
        
        observe({
          performance_metrics <- analysis$performance_metrics
          ratios <- analysis$ratios
          selections_performance <- analysis$selections_performance
          confidence_intervals <- analysis$confidence_intervals
          
          req(performance_metrics, ratios, cancelOutput = TRUE)
          
          output$table <- DT::renderDataTable({
            #selections_performance <- input$selected
            switch_ratio <- FALSE
            round_digits_percentage <- 2
            
            if (length(selections_performance) == 0) {
              df = data.frame(metrics=c("No metric selected"), values=c(""))
            } else {
              
              CI <- confidence_intervals[selections_performance]

              if (switch_ratio) {
                values <- performance_metrics[selections_performance]
                values <- lapply(values, function(x){round(x, round_digits_percentage)})
                
                # format to ratios
                keys_to_format <- intersect(selections_performance, union(valid_ratios, valid_percentage))
                if (length(keys_to_format) > 0) {
                  ratios <- ratios[keys_to_format]
                  values[keys_to_format] <- lapply(ratios, function(x){sprintf("%i/%i", x$k, x$n)})
                }
              } else {
                values <- performance_metrics[selections_performance]
                
                # format to percentage
                keys_to_format <- intersect(selections_performance, union(valid_ratios, valid_percentage))
                if (length(keys_to_format) > 0) {
                  # multipy by 100 before rounding
                  values[keys_to_format] <- lapply(values[keys_to_format], function(x){x*100.0})
                  keys_to_format_CI <- intersect(keys_to_format, names(CI))
                  CI[keys_to_format_CI] <- lapply(CI[keys_to_format_CI], function(x){x*100.0})
                  
                  # round
                  values <- lapply(values, function(x){round(x, round_digits_percentage)})
                  # format
                  #fmt <- paste0("%6.", round_digits_percentage, "f %%")
                  #values[keys_to_format] <- sprintf(fmt, values[keys_to_format])
                  
                  values[keys_to_format] <- paste0(values[keys_to_format], '%')
                } else {
                  values <- lapply(values, function(x){round(x, round_digits_percentage)})
                }
              }
              
              na_keys <- CI == "NULL"
              CI[na_keys] <- ""
              CI[!na_keys] <- lapply(CI[!na_keys], function(x){round(x, round_digits_percentage)})
              CI[!na_keys] <- lapply(CI[!na_keys], function(x){sprintf("%s-%s", x[1], x[2])})
              CI[keys_to_format_CI] <- paste0(CI[keys_to_format_CI], '%')

              
              df = data.frame(
                metrics =  unlist(labels[selections_performance], use.names = FALSE),
                values = unlist(values, use.names = FALSE),
                confidence =unlist(CI, use.names = FALSE)
              )
            }
            #browser()
            conf <- round(analysis$conf.level*100)
            colnames(df) <- c("Metrics", "Values", sprintf("confidence interval [%i%%]", conf))
            
            #tips <- labels
            tips <- tips[selections_performance]
            tips <- paste(paste0("'", tips, "'"), collapse=", ")
            callback = DT::JS(sprintf(
              "var tips = [%s],
            firstColumn = $('#%s td:nth-child(1n+1)');
            for (var i = 0; i < tips.length; i++) {
              $(firstColumn[3*i]).attr('title', tips[i]);
              $(firstColumn[3*i+1]).attr('title', tips[i]);
              $(firstColumn[3*i+2]).attr('title', tips[i]);
            }", tips, paste0(id, '-table')))
            
            DT::datatable(df, rownames=FALSE, callback=callback,
                          selection=list(mode="none", target="row"),
                          options=list(dom='t',
                                       searching=FALSE,
                                       paging=FALSE,
                                       info=FALSE,
                                       ordering=FALSE,
                                       columnDefs=list(list(targets=1:2, class="dt-right")) 
                          )
            ) # %>% DT::formatRound(c(2), 2)
          }, server=FALSE) #DT::renderDataTable
        })
        
        
        
        
        # toggle performance selection
        observeEvent(input$toggle_selection, {
          if (input$toggle_selection) {
            shinyjs::showElement("selected")
          } else {
            shinyjs::hideElement("selected")
          }
        })
      }
    ) # moduleServer
} # performanceServer


# Test ------------------------------------------------------------------------
if (sys.nframe() == 0L) { # if __name__ == '__main__'
  print("Run TEST VERSION")
  
  source('statistics.R')
  source('utils.R')
  source('localization.R')

  localization <- Localization$new("localization.xml")
  mapping_abbrv2label <- localization$get_map_attr2attr_from_xpath('metrics/metric', 'abbrv', 'label')
  mapping_abbrv2tips <- localization$get_map_attr2text_from_xpath('metrics/metric', 'abbrv', strip=TRUE)

  ui <- fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(script = "utils.js",
                           functions = c('disableTab','enableTab')),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    actionButton("btn_createReactive", "DEBUG: create reactive Input"),
    h1("Analysis"),
    fluidRow(
      column(4, 
             tableOutput('confusion_matrix' ),
             verbatimTextOutput("text", placeholder = TRUE)
             ),
      performanceUi("performance")
    ) # fluidRow
  )
  
  server <- function(input, output, session) {
    # PREPROCESSING -------------------------------------------------------------
    # set the defaults 

    doAnalysis <- FALSE
    makeReactiveBinding("doAnalysis")
    
    # emulate reactive bindings
    data <- reactiveValues()
    observeEvent(input$btn_createReactive, {
      
      print("Create reactive variables")
      df <- suppressMessages(readr::read_csv("qualitative_both_methods.csv"))
      df <- convert_df_to_boolean(df[, c('reference', 'test')])
      
      data$reference <- df$reference
      data$test <- df$test
      
      doAnalysis <<- TRUE
    })
    # IMPLEMENTATION -----------------------------------------------------------
    
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

    shinyWidgets::updateCheckboxGroupButtons(session, "performance-selected",
                                             choiceNames = unlist(env_anaylsis$labels, use.names = FALSE),
                                             choiceValues = env_anaylsis$keys,
                                             selected = env_anaylsis$keys_default,
                                             checkIcon = list(yes = icon("ok", lib = "glyphicon"))
    )
    
    
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
    
    # section: Analysis : performance table -------------------------------------
    
    observe({
      req(analysis$performance_metrics, analysis$ratios, cancelOutput = TRUE)
      analysis$selections_performance <<- input[["performance-selected"]]
      
      performanceServer("performance", analysis=analysis, env=env_anaylsis)
      
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
sensitivity : {TNR}% CI: {CI_TNR}%
accuracy    : {ACC}% CI: {CI_ACC}%

LR+         : {LRp} CI: {CI_LRp}
LR-         : {LRn} CI: {CI_LRn}

Performance measure
---------------------------------
                                  (1 is the best, 0 is random)
Cohen's kappa (k)                : {CK} CI: {CI_CK} 
Matthews correlation coefficient : {MCC} CI:
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
    
  }
  
  shinyApp(ui, server)
} # IF ELSE