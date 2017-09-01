
#'Clean ABS Meta Data
#'
#' @name  meta_clean
#' @param meta_data This is the ABS excel title page that you read in with the meta data on it.
#' @return A clean dataframe with the meta data.
#' @export
#'

meta_clean <- function(meta_data = NULL){

  meta_data %>% select(-`Time Series Workbook`, -X__2) %>%
    filter(!is.na(X__1)) %>%
    head(max(lengths(.)) - 1) %>%
    tail(max(lengths(.)) - 1) %>%
    `colnames<-`(.[1,]) %>%
    tail(max(lengths(.)) - 1)

}
