require(WordR)
require(flextable)
require(officer)
library(tidyverse)

source('statistics.R')
source('localization.R')

localization <- Localization$new("localization.xml")
mapping_abbrv2label <- localization$get_map_attr2attr_from_xpath('metrics/metric', 'abbrv', 'label')

create_ft_inputtable <- function(selections, metrics) {
  ratio <- list(list(p=metrics$TP+metrics$FN, n=metrics$FP+metrics$TN),
             list(p=metrics$TP+metrics$FP, n=metrics$FN+metrics$TN))
  
  ratio <- sapply(ratio, function(x){sprintf("%i/%i (%.0f/%.0f%%)", x$p, x$n, 100*x$p/(x$p+x$n), 100*x$n/(x$p+x$n))})
  
  df <- data.frame(
    row.names = c("Reference Standard", "Test method"),
    filename=c(selections$reference_file, selections$reference_file),
    column=c(selections$column_reference, selections$column_test),
    positive=c(selections$selected_reference_positive, selections$selected_test_positive),
    negative=c(selections$choices_reference[selections$choices_reference != selections$selected_reference_positive],
               selections$choices_test[selections$choices_test != selections$selected_test_positive]),
    n_samples=c(nrow(selections$df_reference), nrow(selections$df_test)),
    ratio=ratio
  )
  colnames(df) <- c("file name", "column", "positive", "negative", "N(samples)", "ratio: +/- (%)")
  ft <- df %>% 
    flextable() %>%
    vline(j=1, border = fp_border(width = 1)) %>%
    autofit()
  return(ft)
}


create_ft_confusion_matrix <- function(metrics) {
  df <- as.confusionmatrix(metrics)
  
  cols <- colnames(df)
  df$first <- rep("Test", 2)
  df$second <- c("positive", "negative")
  df <- df[, c("first", "second", cols)]
  
  typology <- data.frame(
    col_keys = colnames(df),
    what = c("", "", "Reference", "Reference"),
    measure = c("", "", "positive", "negative"),
    stringsAsFactors=FALSE
  )
  
  ft <- df %>% 
    flextable() %>%
    set_header_labels(rowname="", first="") %>%
    set_header_df(mapping = typology, key = "col_keys" ) %>%
    merge_h(part = "header") %>%
    align(i=1:2, align = "center", part = "header") %>%
    merge_v(j = 1) %>%
    rotate(i=1, j = 1, rotation = "btlr") %>%
    border_remove  () %>%
    vline(j=3, border = fp_border(width = 1)) %>%
    hline(i=1, j=2:4, border = fp_border(width = 1)) %>%
    vline(j=2, border = fp_border(width = 2)) %>%
    hline(i=2, border = fp_border(width = 2), part = "header") %>%
    fix_border_issues() %>%
    autofit()
  return(ft)
}

create_ft_performance_table <- function(performance_metrics,
                                        confidence_intervals,
                                        conf.level=0.95,
                                        selected = c("TPR", "TNR", "PPV", "ACC")) {
  
  CI <- confidence_intervals[selected]
  round_digits_percentage <- 4
  na_keys <- CI == "NULL"
  CI[na_keys] <- ""
  CI[!na_keys] <- lapply(CI[!na_keys], function(x){round(x, round_digits_percentage)})
  CI[!na_keys] <- lapply(CI[!na_keys], function(x){sprintf("%s-%s", x[1], x[2])})
  #CI[keys_to_format_CI] <- paste0(CI[keys_to_format_CI], '%')
  

  df = data.frame(
    metrics =  unlist(mapping_abbrv2label[selected], use.names = FALSE),
    values = unlist(performance_metrics[selected], use.names = FALSE),
    CI = unlist(CI, use.names = FALSE)
  )
  colnames(df) <- c("Metrics", "Values", sprintf("CI [%i%%]", round(conf.level*100)))
  ft <- df %>%
    flextable() %>%
    theme_zebra(odd_body = "#DEEAF6",odd_header = "#5B9BD5") %>%
    color(i=1, color = '#FFFFFF', part = "header") %>%
    autofit()
  return(ft)
}

create_report_list <- function(analysis,
                               selections,
                               selections_performance, 
                               username=Sys.info()[["user"]]) {
  
  metrics <- analysis$metrics
  performance_metrics <- analysis$performance_metrics
  confidence_intervals <- analysis$confidence_intervals
  conf.level <- analysis$conf.level
  
  if (is.null(selections_performance)) {
    performance_metrics$DEBUG <- ""
    selections_performance <- c('DEBUG')
  }
  
  report = list(
    date = format(Sys.time(), format = "%x %H:%M %Z"),
    username = username,
    n_values = sum(unlist(metrics)),
    FT = list(
      ft_input = create_ft_inputtable(selections, metrics),
      ft_error_matrix = create_ft_confusion_matrix(metrics),
      ft_performance = create_ft_performance_table(performance_metrics, 
                                                   confidence_intervals,
                                                   conf.level=conf.level,
                                                   selected = selections_performance)
    )
  )
}

generate_report_docx <- function(report_file, template_file, report) {
  #WordR::renderInlineCode(template_file, report_file)
  renderInlineCode_in_env(template_file, report_file, env=environment()) # Local version
  WordR::body_add_flextables(report_file, report_file, report$FT)
}

#' (MODIFIED) Read Word document with R code blocks, evaluate them and writes the result into another Word document.
#' NEW: allows passing of an eval environment
#'
#' @param docxIn String of length one; path to Word file with R code blocks.
#' @param docxOut String of length one; path for output Word file.
#' @param debug Boolean of length one; If \code{True} then \code{\link[base]{browser}()} is called at the beginning of the function
#' @return Path to the rendered Word file if the operation was successfull.
#' @examples
#' renderInlineCode(
#'   paste(examplePath(),'templates/template1.docx',sep = ''),
#'   paste(tempdir(),'/result1.docx',sep = ''))
#'
renderInlineCode_in_env <- function(docxIn, docxOut, env = environment(), debug = F) {
  if (debug) {
    browser()
  }
  
  doc<-officer::read_docx(docxIn)
  smm<-officer::docx_summary(doc)
  
  styles<-officer::styles_info(doc)
  
  regx<-"^[ ]*`r[ ](.*)`$"
  smm$expr<-ifelse(grepl(regx,smm$text),sub(regx,"\\1",smm$text),NA)
  
  smm$values <- sapply(smm$expr, FUN = function(x) {
    eval(parse(text = x), envir = env)
  })
  
  smm<-smm[!is.na(smm$expr),,drop=F]
  
  i<-3
  for(i in seq_len(nrow(smm))){
    stylei<-switch(ifelse(is.na(smm$style_name[i]), "a", "b"), a = NULL, b = styles$style_name[styles$style_id==paste0(styles$style_id[styles$style_name==smm$style_name[i]&styles$style_type=="paragraph"],"Char")])
    doc <- officer::cursor_reach(doc, keyword = paste0("\\Q",smm$text[i],"\\E")) %>% officer::body_remove() %>%
      officer::cursor_backward() %>% officer::slip_in_text(smm$values[i],pos="after", style = stylei)
  }
  
  print(doc, target = docxOut)
  return(docxOut)
}
