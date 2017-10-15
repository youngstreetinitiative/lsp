#Ross' original bundle sheets function
#' @export

bundle_sheets <- function(DropboxDataFolder = NULL) {


  if (!is.null(DropboxDataFolder)) {
    paths::DropboxDirFN(DropboxOrigin = DropboxDataFolder)

    print(DropboxDir)

    files <- list.files(DropboxDir, pattern = ".xlsx") %>%
      map( ~ list(
        name = str_replace(.x, ".xls", ""),
        file_path =  glue("{DropboxDir}/{.x}"))) %>%
      set_names(map_chr(., "name"))

    master_ls <- files %>%
      map( ~ process_sheet(.x))

    nms <- master_ls %>% map_chr("full_name")

    master_ls <- master_ls %>%
      set_names(nms)

    rdspath <- glue("{DropboxDir}/master_ls.rds")

    write_rds(master_ls, rdspath)

    master_list <<- read_rds(rdspath)

  } else {
    files <- list.files("data-raw") %>%
      map( ~ list(
        name = str_replace(.x, ".xls", ""),
        file_path =  glue("data-raw/{.x}"))) %>%
      set_names(map_chr(., "name"))
    master_ls <- files %>%
      map( ~ process_sheet(.x))

    nms <- master_ls %>% map_chr("full_name")

    master_ls <- master_ls %>%
      set_names(nms)
    write_rds(master_ls, "data/master_ls.rds")
    master_list <<- read_rds("data/master_ls.rds")
  }
}
