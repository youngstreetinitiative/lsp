

#'  Return data frame with the meta data removed
#'
#'  For a typcial sheet 2
#'
#' @name remove_meta
#' @param df ABS data frame
#' @return a data frame
#' @export
#'
remove_meta <- function(df) {
  meta_lower_bound <-  which(str_detect(df[1], "Series ID")) + 1
  df[meta_lower_bound:max(lengths(df)),]
}
