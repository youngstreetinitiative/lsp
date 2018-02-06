
#' @export

read_excel_multi <- function(input, sheet) {
  if (sheet == 1) {
    read_excel(input, sheet = sheet + 1,
               col_names = F)
  } else {
    read_excel(input, sheet = sheet + 1,
               col_names = F) %>%
      .[,-1]
  }
}

#this function checks the name of the first sheet and then determines what kind of
#ABS data-set you are working with
sheet_check <- function(path = NULL) {
  x <- excel_sheets(path) %>%
    paste(., collapse = "")
  if (str_detect(x, "Content")) {
    sh_type <- "Multi_Table"
  } else if (str_detect(x, "Index")) {
    sh_type <- "Index"
  } else {
    sh_type <- "Other"
  }
  return(sh_type)
}

#this function takes the vetor of list meta information and return the title/umbrella
#information of the ABS data-set so that cat-no and other info can be derived
umbrella <- function(meta_info = NULL) {

  if (meta_info$table_type == "Multi_Table") {
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

#this creates a vector of sheet names from your ABS data-set
sheet_namer <- function(master_list = NULL, var_meta = NULL) {

  if (master_list$table_type == "Index") {
    sheet_names <- master_list$file_path %>%
      excel_sheets() %>%
      .[!. %in% c("Index", "Inquiries")]

  } else if (master_list$table_type == "Multi_Table") {
    sheet_names <- master_list$file_path %>%
      excel_sheets() %>%
      .[!. %in% c("Contents", "Content", "Explanatary Notes")]
  }

  return(sheet_names)
}


work_table <- function(master_list = NULL, sheet_names = NULL) {

  if (master_list$table_type == "Index") {
    if (length(sheet_names) == 1) {
      working_tbl <- master_list$file_path %>%
        read_excel(sheet = 2) %>%
        remove_meta()
    } else {
      working_tbl <- sheet_names %>%
        seq_along() %>%
        map( ~ read_excel_multi(master_list$file_path, sheet = .x)) %>%
        bind_cols() %>%
        remove_meta()
    }
  } else if (master_list$table_type == "Multi_Table") {

    if (length(sheet_names) == 1) {
      working_tbl <- master_list$file_path %>%
        read_excel(sheet = 2)
    } else {
      working_tbl <- sheet_names %>%
        seq_along() %>%
        map( ~ read_excel(master_list$file_path, sheet = .x))
    }
  }
}

# Function for adaption financial year variables to dates (at the end of the financial year)
FYtoDate <- function(FinancialYear = FinancialYear){
  Date <- as.Date(paste0(as.numeric((str_extract(FinancialYear, "[0-9]+"))) + 1, "-06-01"))
  return(Date)
}
