# List of abs names and urls

abs_urls <- list(
  `5655.01` = list(
    file_name = "565501",
    url = "http://www.abs.gov.au/AUSSTATS/abs@archive.nsf/log?openagent&565501.xls&5655.0&Time%20Series%20Spreadsheet&1045F58FB7109D3ECA2581380017AD0B&0&Mar%202017&08.06.2017&Latest"
  ),
  `6345.01` = list(
    file_name = "634501",
    url = "http://www.abs.gov.au/ausstats/meisubs.NSF/log?openagent&634501.xls&6345.0&Time%20Series%20Spreadsheet&8D0AAD2CCDE7F971CA25817D0019FDE8&0&Jun%202017&16.08.2017&Latest"
  )
)

#'Collect Multiple ABS Data Sets
#'
#' @name download_sheets
#' @param url_ls This is a list from within the lsp package of all the ABS URLs that are to be retrieved
#' @return Saves xls files of the spreadsheets to the lsp package folder
#' @export
#'


download_sheets <- function(url_ls = lsp:::abs_urls) {

  url_ls %>%
    walk(
      ~ download.file(.x$url, glue("{getwd()}/data-raw/{.x$file_name}.xls"), mode = "wb")
    )

}

#'Forumalte Important Meta Data from an ABS Data Set
#'
#' @name tbl_meta
#' @param url_ls This is a list from within the lsp package of all the ABS URLs that are to be retrieved and read
#' @return A Data list of dataframes of the meta-data from each ABS data set
#' @importFrom qdapRegex ex_between
#' @export
#'

tbl_meta <- function(url_ls = lsp:::abs_urls){

  #collect_data_multi()
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
