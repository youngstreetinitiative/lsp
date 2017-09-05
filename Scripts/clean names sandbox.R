


files <- list.files("examples") %>%
  map( ~ list(
    name = str_replace(.x, ".xls", ""),
    file_path =  glue("examples/{.x}")
  )) %>%
  set_names(map_chr(., "name"))




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
    str_split("-") %>%
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
                      "\\._" = "_"
                      ))
}


nms <- map_chr(files, "file_path") %>%
  map( ~ read_excel(.x, sheet = 2) %>%
         colnames())


nms %>%
  map( ~ clean_names_full(.x))
