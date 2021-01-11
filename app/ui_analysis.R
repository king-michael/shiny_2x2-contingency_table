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
  
  tagList(
    column(4, 
           DT::dataTableOutput(ns('table')),
           shinyWidgets::materialSwitch(
             inputId = ns("toggle_selection"),
             label = "Change Metrics", 
             value = FALSE,
             status = "info"
           ), # materialSwitch
    ),
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
    )
  ) # tagList
} # reportUi


performanceServer <- function(id,
                              selections_performance,
                              performance_metrics,
                              ratios,
                              env) {
    moduleServer(
      id,
      function(input, output, session) {
        valid_ratios <- env$valid_ratios
        valid_percentage <- env$valid_percentage
        labels <- env$labels
        tips <- env$tips
        
        observe({
          req(performance_metrics, ratios, cancelOutput = TRUE)
          output$table <- DT::renderDataTable({
            req(performance_metrics, ratios, cancelOutput = TRUE)
            
            #selections_performance <- input$selected
            switch_ratio <- FALSE
            round_digits_percentage <- 2
            
            
            if (length(selections_performance) == 0) {
              df = data.frame(metrics=c("No metric selected"), values=c(""))
            } else {
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
                  values <- lapply(values, function(x){round(x, round_digits_percentage)})
                  
                  fmt <- paste0("%6.", round_digits_percentage, "f %%")
                  values[keys_to_format] <- sprintf(fmt, values[keys_to_format])
                } else {
                  values <- lapply(values, function(x){round(x, round_digits_percentage)})
                }
              }
              
              df = data.frame(
                metrics =  unlist(labels[selections_performance], use.names = FALSE),
                values = unlist(values, use.names = FALSE)
              )
            }
            
            colnames(df) <- c("Metrics", "Values")
            
            #tips <- labels
            tips <- tips[selections_performance]
            tips <- paste(paste0("'", tips, "'"), collapse=", ")
            callback = DT::JS(sprintf(
              "var tips = [%s],
            firstColumn = $('#%s td:nth-child(1n+1)');
            for (var i = 0; i < tips.length; i++) {
              $(firstColumn[2*i]).attr('title', tips[i]);
              $(firstColumn[2*i+1]).attr('title', tips[i]);
            }", tips, paste0(id, '-table')))
            
            DT::datatable(df, rownames=FALSE, callback=callback,
                          selection=list(mode="none", target="row"),
                          options=list(dom='t',
                                       searching=FALSE,
                                       paging=FALSE,
                                       info=FALSE,
                                       ordering=FALSE,
                                       columnDefs=list(list(targets=1, class="dt-right")) 
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
      column(4, tableOutput('confusion_matrix' )),
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
        keys = c("TPR", "TNR", "ACC", 'LR+', 'LR-', "PPV", "NPV", "FNR", "FPR", "FDR", "FOR", "F1", "MCC"),
        valid_ratios = c("TPR", "TNR", "PPV", "NPV", "FNR", "FPR", "FDR", "FOR"),
        valid_percentage = c("ACC"),
        keys_default = keys_default <- c("TPR", "TNR", "ACC", 'LR+', 'LR-', "PPV", "NPV"),
        labels = mapping_abbrv2label[keys], # unlist(mapping_abbrv2label[keys], use.names = FALSE)
        tips = mapping_abbrv2tips[keys]
        ),
      parent = emptyenv())

    shinyWidgets::updateCheckboxGroupButtons(session, "performance-selected",
                                             choiceNames = unlist(labels, use.names = FALSE),
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
      
      # draw the confusion matrix
      output$confusion_matrix <- renderTable(as.confusionmatrix(analysis$metrics), rownames=TRUE)
      doAnalysis <<- FALSE
    }, ignoreInit = TRUE)
    
    # section: Analysis : performance table -------------------------------------
    
    observe({
      req(analysis$performance_metrics, analysis$ratios, cancelOutput = TRUE)
      analysis$selections_performance <<- input[["performance-selected"]]
      
      performanceServer("performance", 
                        selections_performance = analysis$selections_performance,
                        performance_metrics = analysis$performance_metrics,
                        ratios = analysis$ratios,
                        env=env_anaylsis)
      
    })
    
    
  }
  
  shinyApp(ui, server)
} # IF ELSE