library(testthat)
source('../../app/utils.R')


test_that("convert_vector_to_boolean", {
  expect_equal(convert_vector_to_boolean(c(1,2)), c(TRUE, FALSE))
  expect_equal(convert_vector_to_boolean(c(1,2), true_value = 2), c(FALSE, TRUE))
  expect_equal(convert_vector_to_boolean(c(1,1), true_value = 2, use_levels=c(1,2)), c(FALSE, FALSE))
})

test_that("convert_vector_to_boolean", {
  df <- data.frame(
    x=c(1,2),
    y=c(1,1),
    z=c(2,2)
  )
  
  expect_equal(convert_df_to_boolean(df)$x, c(TRUE, FALSE))
  expect_equal(convert_df_to_boolean(df, true_value = 2)$x, c(FALSE, TRUE))
  expect_equal(convert_df_to_boolean(df)$y, c(TRUE, TRUE))
  expect_equal(convert_df_to_boolean(df)$z, c(FALSE, FALSE))
})
