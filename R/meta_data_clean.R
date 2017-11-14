
#'Clean ABS Meta Data
#'
#' @name  meta_clean
#' @param meta_data This is the ABS excel title page that you read in with the meta data on it.
#' @param table_type This is a character vector of the kind of ABS table it is Index or Multi
#' @return A clean dataframe with the meta data.
#' @export
#'


meta_clean <- function(meta_data = NULL, table_type = NULL){

  if (table_type == "Multi Table") {

    meta_data %>% select(-`Australian Bureau of Statistics`) %>%
      filter(!is.na(X__1), !is.na(X__2)) %>%
      `colnames<-`(c("Table", "Content"))


  } else if (table_type == "Index") {

    meta_data %>% select(-`Time Series Workbook`, -X__2) %>%
      filter(!is.na(X__1)) %>%
      head(max(lengths(.)) - 1) %>%
      tail(max(lengths(.)) - 1) %>%
      `colnames<-`(.[1,]) %>%
      `colnames<-`(str_replace_all(colnames(.), " ", "_")) %>%
      `colnames<-`(str_replace_all(colnames(.), "\\.", "")) %>%
      tail(max(lengths(.)) - 1)
  }

}





# meta_clean <- function(meta_data = NULL){
#
#   meta_data %>% select(-`Time Series Workbook`, -X__2) %>%
#     filter(!is.na(X__1)) %>%
#     head(max(lengths(.)) - 1) %>%
#     tail(max(lengths(.)) - 1) %>%
#     `colnames<-`(.[1,]) %>%
#     `colnames<-`(str_replace_all(colnames(.), " ", "_")) %>%
#     `colnames<-`(str_replace_all(colnames(.), "\\.", "")) %>%
#     tail(max(lengths(.)) - 1)
#
# }
