

#'Collect Multiple ABS Data Sets
#'
#' @name collect_data_multi
#' @param url_ls This is a list from within the lsp package of all the ABS URLs that are to be retrieved
#' @return Saves xls files of the spreadsheets to the lsp package folder
#' @export
#'

collect_data_multi <- function(url_ls = lsp:::abs_urls) {

  url_ls %>%
    walk(
      ~ download.file(.x$url, glue("{getwd()}/{.x$file_name}.xls"), mode = "wb")
    )

  # url_ls %>%
  #   map(
  #     ~ read_xls(glue("{getwd()}/{.x$file_name}.xls"), sheet = 2)
  #   )

}


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
