#' @export

bundle_sheets <- function() {

  files <- list.files("data-raw") %>%
    map( ~ list(
      name = str_replace(.x, ".xls", ""),
      file_path =  glue("data-raw/{.x}")
    )) %>%
    set_names(map_chr(., "name"))

  master_ls <- files %>%
    map( ~ process_sheet(.x))

  nms <- master_ls %>% map_chr("full_name")

  master_ls <- master_ls %>%
    set_names(nms)

  write_rds(master_ls, "data/master_ls.rds")

}
