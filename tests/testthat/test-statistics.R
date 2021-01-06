library(testthat)
source('../../app/statistics.R')
source('../utils.R')

test_that("calculate_metrics", {
  #             # TP    TP    TP    FN    FP     FP    TN      TN     TN     TN
  reference <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  test <-      c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE,  FALSE, FALSE, FALSE, FALSE)
  expected_values = list(TP=3, FN=1, FP=2, TN=4)
  expect_equal(compare_named_unordered_lists(calculate_metrics(reference, test), expected_values), TRUE)
})
