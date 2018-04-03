#' @export

Bundle_Sheets <- function(DropboxDataFolder = NULL, InclVarMeta = FALSE, LookForIndustry = FALSE) {


  if (!is.null(DropboxDataFolder)) {
    paths::DropboxDirFN(DropboxOrigin = DropboxDataFolder)


    files <- list.files(DropboxDir, pattern = ".xls") %>%
      map( ~ list(
        name = gsub(("\\.xlsx|\\.xls"), "", .x),
        file_path =  glue("{DropboxDir}/{.x}"),
        table_type = lsp:::sheet_check(glue("{DropboxDir}/{.x}")))) %>%
      set_names(map_chr(., "name"))

    master_ls <- files %>%
      map( ~ lsp:::process_sheets(.x,
                                     InclVarMeta = InclVarMeta,
                                     LookForIndustry = LookForIndustry))

    # Pulling up full_name from each list element and then saving as name of the element
    nms <- master_ls %>%
      map_chr("full_name")
    master_ls <- master_ls %>%
      set_names(nms)

    # Still don't know what this part does
    rdspath <- glue("{DropboxDir}/master_ls.rds")
    write_rds(master_ls, rdspath)
    master_list <<- read_rds(rdspath)

  } else {
    files <- list.files("data-raw") %>%
      map( ~ list(
        name = gsub(("\\.xlsx|\\.xls"), "", .x),
        file_path =  glue("data-raw/{.x}"),
        table_type = lsp:::sheet_check(glue("data-raw/{.x}")))) %>%
      set_names(map_chr(., "name"))
    master_ls <- files %>%
      map( ~ lsp:::process_sheet_new(.x, InclVarMeta = InclVarMeta, LookForIndustry = LookForIndustry))

    nms <- master_ls %>% map_chr("full_name")

    master_ls <- master_ls %>%
      set_names(nms)
    write_rds(master_ls, "data/master_ls.rds")
    master_list <<- read_rds("data/master_ls.rds")
  }
}
