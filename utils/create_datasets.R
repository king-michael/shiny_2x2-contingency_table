

#' Creates a dataset with qualitative values (`TRUE`/`FALSE`)
#'
#' @param n_samples Number of samples (integer)
#' @param ratio_negative Ratio negative values (float)
#' @param pertubation Ratio of how many random values should be pertubated (float)
#'
#' @return `data.frame(reference, test)`
#' @export
#'
#' @examples
#' df = create_qualitative_testset(100, 0.8, 0.1)
#' isTRUE(sum(!df$reference)/length(df$reference) == 0.8)
create_qualitative_testset <- function(n_samples, ratio_negative=0.2, pertubation=0.1){
  # create the reference set
  n_negative <- floor(n_samples*(ratio_negative))
  reference <- rep(TRUE, n_samples)
  reference[sample.int(n_samples, size=n_negative, replace = FALSE)] <- FALSE
  
  
  # create the test set
  n_pertubate <- floor(n_samples*pertubation)
  test <- reference
  test[sample.int(n_samples, size=n_pertubate, replace = FALSE)] <- as.logical(sample(0:1, n_pertubate, replace = TRUE))
  
  return(data.frame(reference=reference, test=test))
}



#' Create test set based on true_positive, true_negative, false_positive, false_negative values.
#'
#' @param true_positive Number of true positives. (integer)
#' @param true_negative Number of true negatives. (integer)
#' @param false_positive Number of false positives. (integer)
#' @param false_negative Number of false negatives. (integer)
#' @param shuffle : Boolean if the set should be shuffled or not. (logical)
#'
#' @return
#' @export
#'
#' @examples
#' df <- create_qualitative_testset.bymetrics(
#' true_positive = 100,
#' true_negative = 50,
#' false_positive = 13,
#' false_negative = 7
#' )
#' isTRUE(sum(df$reference) == (100 + 7))
#' isTRUE(sum(!df$reference) == (50 + 13))
#' isTRUE(sum(df$test) == (100 + 13))
#' isTRUE(sum(!df$test) == (50 + 7))
create_qualitative_testset.bymetrics <- function(true_positive,
                                                 true_negative,
                                                 false_positive,
                                                 false_negative,
                                                 shuffle = FALSE) {
  reference <- c(
    rep(TRUE, true_positive),
    rep(FALSE, true_negative),
    rep(TRUE, false_negative),
    rep(FALSE, false_positive)
  )
  test <- c(
    rep(TRUE, true_positive),
    rep(FALSE, true_negative),
    rep(FALSE, false_negative),
    rep(TRUE, false_positive)
  )
  
  if (shuffle) {
    print("shuffle")
    index <- sample(1:length(reference))
    reference <- reference[index]
    test <- test[index]
  }
  return(data.frame(reference = reference, test = test))
}
