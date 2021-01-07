

is_valid_column <- function(values) {
  # short sample to speed up big datasets
  if (length(values) > 1000) {
    n_unique <- length(unique(values[1:10])) 
    if (n_unique > 2) {return(FALSE)}
  }
  
  n_unique <- length(unique(values))
  return(n_unique <= 2)
}


#' Converts a vector to boolean values (using factors)
#'
#' @param x Vector of values
#' @param true_value Value reflecting `TRUE`. Default is `NULL`. (optional)
#' @param use_levels Vector of levels to use.  Default is `NULL`. (optional)
#'
#' @return
#' @export
#'
#' @examples
#' convert_vector_to_boolean(c(1,2,1,2), true_value=2)
#' convert_vector_to_boolean(c(2,2), true_value=2, use_levels=c(1,2))
convert_vector_to_boolean <- function(x, true_value = NULL, use_levels = NULL){
  
  if (is.null(use_levels)){
    x <- as.factor(x)
  } else {
    x <- factor(x, levels=as.character(use_levels))
  }
  
  if (is.null(true_value)){
    levels(x) <- c(TRUE, FALSE)
  } else {
    levels(x) <- levels(x) == true_value
  }
  x <- as.logical(x)
  return(x)
}

#' Converts a data frame of values to qualitative values (`TRUE`/`FALSE`).
#'
#' @param df Dataframe
#' @param true_value Value reflecting `TRUE`. Default is `NULL`. (optional)
#'
#' @return
#' @export
#'
#' @examples
#' 
#' convert_df_to_boolean( data.frame(x=c(1,2),y=c(1,1)) )
convert_df_to_boolean <- function(df, true_value = NULL){
  # get unique levels
  lvls <- NULL
  for (i in names(df)) {
    lvls <- union(lvls, unique(df[[i]]))
  }
  lvls <- as.character(lvls)
  
  # convert the different columns
  for (i in names(df)) {
    df[[i]] <- convert_vector_to_boolean(df[[i]], true_value = true_value, use_levels = lvls)
  }
  
  return(df)
}
