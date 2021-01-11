require(irr)
require(GenBinomApps)


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


get_ratios <- function(TP, TN, FP, FN) {
  ratios <- list(
    TPR = list(k = TP, n = (TP + FN)), # true positive rate / recall / sensitivity
    TNR = list(k = TN, n = (TN + FP)), # true negative rate / specificity / selectivity
    ACC = list(k = (TP + TN), n = (TP + FN + TN + FP)), # accuracy
    
    "LR+" = list(k = (TP / (TP + FN)), n = (FP / (TN + FP))), # positive likelihood ratio
    "LR-" = list(k = (FN / (TP + FN)), n = (TN / (TN + FP))), # negative likelihood ratio
    
    PPV = list(k = TP, n = (TP + FP)), # positive predictive value / precision
    NPV = list(k = TN, n = (TN + FN)), # negative predictive value 
    
    FNR = list(k = FN, n = (TP + FN)), # miss rate or false negative rate
    FPR = list(k = FP, n = (TN + FP)), # fall-out or false positive rate 
    
    FDR = list(k = FP, n = (FP + TP)), # false discovery rate
    FOR = list(k = FN, n = (FN + TN)) # false omission rate
  )
  return(ratios)
}

#' List with the individual functions 
pm_functions <- list(
  F1 = function(TP, TN, FP, FN) {(2 * TP) / (2*TP + FP + FN)}, # F1 score
  MCC = calculate_MCC # Matthews correlation coefficient
)


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
  
  pm <- list(
    TP = TP, TN = TN, FN = FN, FP = FP,
    P = P, N = N
  )
  pm <- append(pm, lapply(get_ratios(TP, TN, FP, FN), function(r){r$k / r$n}))
  pm <- append(pm, lapply(pm_functions, function(FUNC){FUNC(TP, TN, FP, FN)}))
  
  return(pm)
}
# calculate_performance_metrics.old <- function(TP, TN, FP, FN) {
#   P <- TP + FN  # condition positive
#   N <- TN + FP  # condition negative
#   
#   metrics <- list(
#     TP = TP, TN = TN, FN = FN, FP = FP,
#     P = P, N = N,
#     TPR = TP / P ,  # true positive rate / recall / sensitivity
#     TNR = TN / N,  # true negative rate / specificity / selectivity
#     PPV = TP / (TP + FP), # positive predictive value / precision
#     NPV = TN / (TN + FN), # negative predictive value 
#     FNR = FN / P, # miss rate or false negative rate
#     FPR = FP / N, # fall-out or false positive rate 
#     FDR = FP / (FP + TP), # false discovery rate
#     FOR = FN / (FN + TN), # false omission rate
#     ACC = (TP + TN) / (P + N), # accuracy
#     F1 = (2 * TP) / (2*TP + FP + FN), # F1 score
#     MCC = calculate_MCC(TP, TN, FP, FN) # Matthews correlation coefficient
#   )
#   
#   return(metrics)
# }


#' Map a, b, c, d to TP, TN, FP, FN
#'     +   -          +    -
#' + | a | b |    + | TP | FP |
#'   |---+---| ->   |----+----|
#' - | c | d |    - | FN | TN |
#' 
#' @param a 
#' @param b 
#' @param c 
#' @param d 
#'
#' @return
#' @export
#'
#' @examples
map_abcd2names <- function(a,b,c,d) {
  list(TP=a, TN=d, FP=b, FN=c)
}


#' Calculate the confidence intervals of the different performance measures
#'
#' @param TP True positive (integer)
#' @param TN True negative (integer)
#' @param FP False positive (integer)
#' @param FN False negative (integer)
#' @param conf.level 
#'
#' @return
#' @export
#'
#' @examples
calculate_confidence_intervals <- function(TP, TN, FP, FN, 
                                           conf.level = 0.95) {
  
  alpha <- 1 - conf.level
  z.score <- qnorm(1-alpha/2)

  clopper.pearson <- function(k, n, alpha) {
    r <- GenBinomApps::clopper.pearson.ci(k, n, alpha=alpha, CI="two.sided")
    CI <- c(r$Lower.limit, r$Upper.limit)
    return(CI)
  }
  
  log.CI <- function(TP, TN, FP, FN, key, z.score) {
    
    # check proposed by altman p110
    if (any(TP == 0, FP == 0, FN == 0, TN==0)) {
      TN <- TN+0.5
      FP <- FP+0.5
      FN <- FN+0.5
      TN <- TN+0.5
    }
    
    list_D <- list(
      "LR+" = ( (TP / (TP+FN)) / (FP / (TN+FP)) ), # positive likelihood ratio
      "LR-" = ( (FN / (TP+FN)) / (TN / (TN+FP)) )  # negative likelihood ratio
    )
    list_SE <- list(
      "LR+" = sqrt(1/TP - 1/(TP+FN) + 1/FP - 1/(FP+TN)),# positive likelihood ratio
      "LR-" = sqrt(1/FN - 1/(TP+FN) + 1/TN - 1/(FP+TN)) # negative likelihood ratio
    )
    if (!(key %in% names(list_D)) || !(key %in% names(list_SE))) {
      stop("ERROR : no forumal for this key")
    }
    
    D <- list_D[[key]]
    SE <- list_SE[[key]]
    
    CI <- exp(c(log(D)-z.score*SE, log(D)+z.score*SE))
    
    return(CI)
  }
  
  standard.CI <- function(D, SE, z.score) { c(D-z.score*SE, D+z.score*SE) }
  standard.calculate_SE <- function(k, n) {
    D <- k / n
    SE <- sqrt((D*(1-D))/n)
    return(SE)
  }
  
  ratios <- get_ratios(TP, TN, FP, FN)
  pm <- calculate_performance_metrics(TP, TN, FP, FN)
  
  CI <- list(
    TPR = do.call(clopper.pearson, c(ratios$TPR, alpha)),
    TNR = do.call(clopper.pearson, c(ratios$TNR, alpha)),
    ACC = do.call(clopper.pearson, c(ratios$ACC, alpha)),
    
    "LR+" = do.call(log.CI, list(TP, TN, FP, FN, 'LR+', z.score)),
    "LR-" = do.call(log.CI, list(TP, TN, FP, FN, 'LR-', z.score))
  )
  for (key in setdiff(names(ratios), names(CI))) {
    CI[[key]] <- standard.CI(D = pm[[key]],
                             SE = do.call(standard.calculate_SE, ratios[[key]]),
                             z.score = z.score)
  }
  
  
  return(CI)
}


add_additional_metrics <- function(pm, CI, df, conf.level=0.95) {
  alpha <- 1 - conf.level
  
  k <- psych::cohen.kappa(df, alpha=alpha)
  pm[["CK"]] <- k$kappa
  CI[["CK"]] <- unlist(k$confid[1,c(1,3)], use.names = FALSE)
  
  return(list(pm=pm, CI=CI))
}

