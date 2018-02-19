

#'  Clean ABS variable names (full)
#'
#' @name clean_names_full
#' @param names_vec Character vector (eg. \code{colnames(df)})
#' @return A character vector
#' @export
#'

clean_names_full <- function(names_vec) {
  names_vec %>%
    str_trim %>%
    str_replace_all(c("_" = ".")) %>%
    str_replace_all(c("\\s+;" = "_",
                      ":\\s+" = "_",
                      "\\s+\\-\\s+" = "_",
                      "\\-\\s+" = "")) %>%
    str_replace_all(c("\\(" = "",
                      "\\)" = "",
                      "\\$" = "",
                      "\\>\\s" = "",
                      "\\>" = "",
                      ":" = "",
                      "," = "",
                      ";" = "")) %>%
    str_replace_all(c("\\s+" = ".",
                      "/" = ".",
                      "\\-" = ".")) %>%
    str_replace_all(c("\\._\\." = "_",
                      "_\\." = "_",
                      "\\._" = "_")) %>%
    str_replace_all(c("___" = "_",
                      "__" = "_")) %>%
    map( ~ str_replace(.x," *[^a-zA-Z0-9]$", "")) %>% ## if the string ends in a non-alphanumeric, remove it
    unlist()
}

