#' Compares to named lists. Ignores order of names
#'
#' @param list1 First list
#' @param list2 Second list
#'
#' @return
#' @export
#'
#' @examples
#' compare_named_unordered_lists(list(a=1, b=2), list(a=1, b=2))
#' @tests
#' expect_equal(compare_named_unordered_lists(list(a=1, b=2), list(a=1, b=2)), TRUE)  # ordered
#' expect_equal(compare_named_unordered_lists(list(a=1, b=2), list(b=2, a=1)), TRUE)  # unordered
#' expect_equal(compare_named_unordered_lists(list(a=1, b=2), list(a=1)), FALSE)      # different size
#' expect_equal(compare_named_unordered_lists(list(a=1), list(a=1, b=2)), FALSE)      # different size
#' expect_equal(compare_named_unordered_lists(list(), list()), TRUE)                  # empty list                  
#' expect_equal(compare_named_unordered_lists(list(a=1), list()), FALSE)              # empty list
#' expect_equal(compare_named_unordered_lists(list(a=1, b=2), list(a=1, b=3)), FALSE) # different values
#' expect_equal(compare_named_unordered_lists(list(a=1, b=2), list(a=1, c=2)), FALSE) # different names
compare_named_unordered_lists <- function(list1, list2){
  if (!setequal(ordered(names(list1)),
                ordered(names(list2))) ) {
    return(FALSE)
  }

  checks <- sapply(names(list1), function(i){all.equal(list1[[i]], list2[[i]])})
  return(all(checks))
}
