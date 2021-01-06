

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
