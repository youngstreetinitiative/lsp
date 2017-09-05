
#' @export

read_excel_multi <- function(input, sheet) {
  if(sheet == 1){
    read_excel(input, sheet = sheet + 1)
  } else {
    read_excel(input, sheet = sheet + 1) %>%
      .[,-1]
  }
}

