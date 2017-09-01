

#'  Clean ABS variable names (full)
#'
#' @name clean_names_full
#' @param names_vec Character vector (eg. \code{colnames(df)})
#' @return A character vector
#' @export

clean_names_full <- function(names_vec) {
  original_nms %>%
    str_replace_all(";", "") %>%
    str_trim() %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("__","_") %>%
    str_replace_all("__","_") %>%
    str_replace_all(c("\\(" = "", "\\)" = ""))
}


