
#' @export

read_excel_multi <- function(input, sheet) {
  if (sheet == 1) {
    read_excel(input, sheet = sheet + 1)
  } else {
    read_excel(input, sheet = sheet + 1) %>%
      .[,-1]
  }
}

#this function checks the name of the first sheet and then determines what kind of
#ABS data-set you are working with
sheet_check <- function(path = NULL) {
  x <- excel_sheets(path) %>%
    paste(., collapse = "")
  if (str_detect(x, "Content")) {
    sh_type <- "Multi Table"
  } else if (str_detect(x, "Index")) {
    sh_type <- "Index"
  } else {
    sh_type <- "Other"
  }
  #return(sh_type)
}

#this function takes the vetor of list meta information and return the title/umbrella
#information of the ABS data-set so that cat-no and other info can be derived
umbrella <- function(meta_info = NULL) {

  if (meta_info$table_type == "Multi Table") {
    meta_tbl_umbrella <- meta_info$file_path %>%
      read_excel(sheet = 1) %>%
      select(`Australian Bureau of Statistics`) %>%
      filter(!is.na(`Australian Bureau of Statistics`)) %>%
      pull(1)

  } else if (meta_info$table_type == "Index") {
    meta_tbl_umbrella <- meta_info$file_path %>%
      read_excel(sheet = 1) %>%
      select(`Time Series Workbook`) %>%
      filter(!is.na(`Time Series Workbook`)) %>%
      pull(1)
  } else {
    break("This is not a normal ABS Data Set")
  }

}


# abs_read <- function(path = NULL) {
#   if (!is.null(path)) {
#     paths::DropboxDirFN(DropboxOrigin = path)
#     print(DropboxDir)
#     files <- list.files(DropboxDir) %>%
#       map( ~ list(
#         name = gsub(("\\.xlsx|\\.xls"), "", .x),
#         file_path = glue("{DropboxDir}/{.x}"),
#         table_type = lsp:::sheet_check(glue("{DropboxDir}/{.x}")))) %>%
#       set_names(map_chr(., "name"))
#   } else {
#     files <- list.files("data-raw") %>%
#       map( ~ list(
#         name = gsub(("\\.xlsx|\\.xls"), "", .x),
#         file_path =  glue("data-raw/{.x}"),
#         table_type = lsp:::sheet_check(glue("data-raw/{.x}")))) %>%
#       set_names(map_chr(., "name"))
#   }
#   return(files)
# }

#this counts the amount of sheets that your ABS data-set has
sheet_counter <- function(master_list = NULL, var_meta = NULL) {

  if (master_list$table_type == "Index") {
    sheet_count <- master_list$file_path %>%
      excel_sheets() %>%
      .[!. %in% c("Index", "Inquiries")]

  } else if (master_list$table_type == "Multi Table") {
    last_sheet <- tail(var_meta["Table"], 1) %>%
      pull()
    sheet_count <- master_list$file_path %>%
      excel_sheets() %>%
      .[!. %in% c("Content", last_sheet)]
  }

  #message(glue("--- has {length(sheet_count)} sheet/s"))

  return(sheet_count)
}


work_table <- function(master_list = NULL, sheet_count = NULL) {

  if (master_list$table_type == "Index") {
    if (length(sheet_count) == 1) {
      working_tbl <- master_list$file_path %>%
        read_excel(sheet = 2) %>%
        remove_meta()
    } else {
      working_tbl <- sheet_count %>%
        seq_along() %>%
        map( ~ read_excel_multi(master_list$file_path, sheet = .x)) %>%
        bind_cols() %>%
        remove_meta()
    }
  } else if (master_list$table_type == "Multi Table") {

    if (length(sheet_count) == 1) {
      working_tbl <- master_list$file_path %>%
        read_excel(sheet = 2) #%>%
        #t(.)
    } else {
      working_tbl <- sheet_count %>%
        seq_along() %>%
        map( ~ read_excel(master_list$file_path, sheet = .x)) #%>%
        #map( ~ t(.))
    }
  }
}


process_sheet_new <- function(input_ls) {

  message(input_ls$name)

  # parts from Alex's fun to grab details for cover... ====
  meta_tbl_umbrella <- lsp:::umbrella(input_ls)

#  message("Umbrella Done")

  if (input_ls$table_type == "Index") {

    cat_no <- regmatches(meta_tbl_umbrella[1], gregexpr("[[:digit:]]+", meta_tbl_umbrella[1])) %>%
      unlist() %>%
      str_c(collapse = ".")

    Table_Title <- str_trim(str_replace(meta_tbl_umbrella[2], unlist(ex_between(meta_tbl_umbrella[2], "T", ".", include.markers = TRUE)), ""))

    Table_No <- str_replace(unlist(ex_between(meta_tbl_umbrella[2], "T", ".", include.markers = TRUE)), "\\.", "") %>%
      str_replace_all(c(
        "TABLE" = "",
        "Table" = "",
        "table" = ""
      )) %>% str_trim()

    cat_no <- paste0(cat_no,Table_No)


  } else if (input_ls$table_type == "Multi Table") {

    cat_no <- regmatches(meta_tbl_umbrella[1], gregexpr("[[:digit:]]+", meta_tbl_umbrella[1])) %>%
      unlist() %>%
      str_c(collapse = ".")

    Table_Title <- collapse(unlist(regmatches(meta_tbl_umbrella[1], gregexpr("[[:alpha:]]+", meta_tbl_umbrella[1]))), sep = " ")

  }

  full_name <- glue("ABS_{cat_no}_{Table_Title}")


 # message("Full Name done")

  # my parts =====
  var_meta <- input_ls$file_path %>%
    read_excel(sheet = 1) %>%
    meta_clean(table_type = input_ls$table_type)

  #message("Meta Table Done")

  sheet_count <- lsp:::sheet_counter(input_ls, var_meta)

 # print(sheet_count)

  #message(glue("--- has {length(sheet_count)} sheet/s"))

  working_tbl <- lsp:::work_table(input_ls, sheet_count)

  #print(working_tbl)

  #message("Working Table created")

  if (input_ls$table_type == "Index") {
    original_nms <- colnames(working_tbl)
    new_nms <- clean_names_full(original_nms)

    var_meta <- var_meta %>%
      mutate(Data_Item_Description = new_nms[-1])

    new_nms[1] <- "Date"
    colnames(working_tbl) <- new_nms

    working_tbl <- working_tbl %>%
      mutate(Date = as.POSIXct(as.numeric(Date) * (60*60*24), origin = "1899-12-30", format = "%Y-%m-%d", tz = "GMT"))

    split_key <- var_meta %>%
      distinct(Data_Item_Description, Series_Type)

    working_tbl <- working_tbl %>%
      gather(cat, val, 2:length(.)) %>%
      inner_join(split_key, by = c("cat" = "Data_Item_Description")) %>%
      split(.$Series_Type) %>%
      map( ~ spread(.x, cat, val))
  } else if (input_ls$table_type == "Multi Table") {
    working_tbl <- working_tbl

  }

  #print(working_tbl)

  working_list <- list("var_meta" = var_meta, "working_tbl" = working_tbl, "full_name" = full_name)

  # message("Process Sheets Finished")
  #
  # return(working_list)
}
