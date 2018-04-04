
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
#' @export
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
#' @export
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


#this creates a vector of sheet names from your ABS data-set
#' @export
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



# Function for adaption financial year variables to dates (at the end of the financial year)
#' @export
FYtoDate <- function(FinancialYear = FinancialYear){
  Date <- as.Date(paste0(as.numeric((str_extract(FinancialYear, "[0-9]+"))) + 1, "-06-01"))
  return(Date)
}
