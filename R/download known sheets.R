

#'Collect Multiple ABS Data Sets
#'
#' @name download_sheets
#' @param url_ls This is a list from within the lsp package of all the ABS URLs that are to be retrieved
#' @return Saves xls files of the spreadsheets to the lsp package folder
#' @export


download_sheets <- function(url_ls = lsp:::abs_urls) {

  url_ls %>%
    walk(
      ~ download.file(.x$url, glue("{getwd()}/data-raw/{.x$file_name}.xls"), mode = "wb")
    )

}


