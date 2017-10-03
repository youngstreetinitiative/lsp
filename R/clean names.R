

#'  Clean ABS variable names (full)
#'
#' @name clean_names_full
#' @param names_vec Character vector (eg. \code{colnames(df)})
#' @return A character vector
#' @export

clean_names_full <- function(names_vec) {
  names_vec %>%
    str_split(";") %>%
    map( ~ str_trim(.x) %>%
           .[. != ""] %>%
           str_replace_all(" ", ".") %>%
           str_c(collapse = "_")) %>%
    str_split(":") %>%
    map( ~ str_trim(.x) %>%
           .[. != ""] %>%
           str_replace_all(" ", ".") %>%
           str_c(collapse = "_")
    ) %>%
    unlist() %>%
    str_replace_all(c("\\(" = "",
                      "\\)" = "",
                      "," = "",
                      "\\._\\." = "_",
                      "_\\." = "_",
                      "\\._" = "_",
                      ">"="",
                      "\\-"="."
    )) %>%
    str_replace_all(c("___"="_",
                      "\\s+" = ".",
                      "__"="_"))
}

