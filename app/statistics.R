
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
    rownames = c('Test Method+', 'Test Method-'),
    ref_positive = c(metrics$TP, metrics$FN),
    ref_negative = c(metrics$FP, metrics$TN),
    row.names = 'rownames'
  )
  colnames(confusionmatrix) <- c('Reference Method+', 'Reference Method-')
  
  return(confusionmatrix)
}


#' Calculate the Matthews correlation coefficient
#'
#' @param TP True positive (integer)
#' @param TN True negative (integer)
#' @param FP False positive (integer)
#' @param FN False negative (integer)
#'
#' @return
#' @export
#'
#' @examples
calculate_MCC <- function(TP, TN, FP, FN) {
  # leads to really big numbers in the divisor (workaround with double or original formula)
  # MCC <- (TP*TN - FP*FN) / sqrt((as.double(TP)+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  N <- TN+TP+FN+FP
  S <- (TP+FN)/N
  P <- (TP+FP)/N
  MCC <- ((TP/N) - S*P)/sqrt(P*S*(1-S)*(1-P))
  return(MCC)
}


#' Calculate various performance metrics
#'
#' @param TP True positive (integer)
#' @param TN True negative (integer)
#' @param FP False positive (integer)
#' @param FN False negative (integer)
#'
#' @return
#' @export
#'
#' @examples
calculate_performance_metrics <- function(TP, TN, FP, FN) {
  P <- TP + FN  # condition positive
  N <- TN + FP  # condition negative
  
  metrics <- list(
    TP = TP, TN = TN, FN = FN, FP = FP,
    P = P, N = N,
    TPR = TP / P ,  # true positive rate / recall / sensitivity
    TNR = TN / N,  # true negative rate / specificity / selectivity
    PPV = TP / (TP + FP), # positive predictive value / precision
    NPV = TN / (TN + FN), # negative predictive value 
    FNR = FN / P, # miss rate or false negative rate
    FPR = FP / N, # fall-out or false positive rate 
    FDR = FP / (FP + TP), # false discovery rate
    FOR = FN / (FN + TN), # false omission rate
    ACC = (TP + TN) / (P + N), # accuracy
    F1 = (2 * TP) / (2*TP + FP + FN), # F1 score
    MCC = calculate_MCC(TP, TN, FP, FN) # Matthews correlation coefficient
  )
  
  return(metrics)
}

