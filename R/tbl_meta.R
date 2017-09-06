#'Forumalte Important Meta Data from an ABS Data Set
#'
#' @name tbl_meta
#' @param url_ls This is a list from within the lsp package of all the ABS URLs that are to be retrieved and read
#' @return A Data list of dataframes of the meta-data from each ABS data set
#' @importFrom qdapRegex ex_between
#' @export
#'

tbl_meta <- function(url_ls = lsp:::abs_urls){

  collect_data_multi()
  date <- Sys.Date()
  tbl_meta <- list()

  for (i in 1:length(url_ls)) {
    tbl <- read_excel(paste(url_ls[[i]]$file_name,".xls", sep = "")) %>%
      select(`Time Series Workbook`) %>%
      filter(!is.na(`Time Series Workbook`))

    temp <- tbl[[1]]

    tbl_end <- tibble("Date_Accessed" = date,
                      "Catelog_No" = regmatches(temp[1], gregexpr("[[:digit:]]+", temp[1])) %>%
                        unlist() %>%
                        str_c(collapse = "."),
                      "Title" = regmatches(temp[1], gregexpr("[[:alpha:]]+", temp[1])) %>%
                        unlist() %>%
                        str_c(collapse = " "),
                      "Table_Title" = str_trim(str_replace(temp[2], unlist(ex_between(temp[2], "T", ".", include.markers = TRUE)), "")),
                      "Table_No" = str_replace(unlist(ex_between(temp[2], "T", ".", include.markers = TRUE)), "\\.", ""),
                      "Catelog_Table" = as.double(url_ls[[i]]$file_name),
                      "URL" = url_ls[[i]]$url)

    tbl_meta[[i]] <- tbl_end
  }
  tbl_meta
}

