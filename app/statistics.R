
#' Calculates the metrics of a confusion matrix from a reference and test vector.
#'
#' @param reference A vector with the gold standard.
#' @param test A vector with the test values
#'
#' @return
#' @export
#'
#' @examples
#' source("utils.R")
#' df <- create_qualitative_testset(n_samples, ratio_negative, pertubation)
#' metrics <- calculate_metrics(df$reference, df$test)
calculate_metrics <- function(reference, test){
  metrics <- list(
    TP = sum(reference & test),
    TN = sum(!reference & !test),
    FP = sum(!reference & test),
    FN = sum(reference & !test)
  )
  return(metrics)
}


#' Converts a list of metrics to the confusion matrix.
#'
#' @param metrics A named list of metrics (TP, TN, FP, FN)
#'
#' @return
#' @export
#'
#' @examples
#' confusionmatrix <- as.confusionmatrix(list(TP=t100, TN=30, FP=3, FN=10))
as.confusionmatrix <- function(metrics){
  confusionmatrix <- data.frame(
    rownames = c('Test Method +', 'Test Method -'),
    ref_positive = c(metrics$TP, metrics$FP),
    ref_negative = c(metrics$FN, metrics$TN),
    row.names = 'rownames'
  )
  colnames(confusionmatrix) <- c('Reference Method+', 'Reference Method-')
  
  return(confusionmatrix)
}