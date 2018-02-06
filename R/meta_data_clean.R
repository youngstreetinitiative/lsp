
#'Clean ABS Meta Data
#'
#' @name  meta_clean
#' @param meta_data This is the ABS excel title page that you read in with the meta data on it.
#' @param table_type This is a character vector of the kind of ABS table it is Index or Multi
#' @return A clean dataframe with the meta data.
#' @export
#'


meta_clean <- function(meta_data = NULL, table_type = NULL){

  if (table_type == "Multi_Table") {

    # Save the table names and numbers from the Contents sheet along with a cleaned version of the names
    meta_data <- meta_data %>% select(-`Australian Bureau of Statistics`) %>%
      filter(!is.na(X__1), !is.na(X__2)) %>%
      `colnames<-`(c("Table", "Content")) %>%
      mutate(Table.Name = clean_names_full(Content))


  } else if (table_type == "Index") {

    # Save the vaiable information from the Index sheet
    # transform the Date variables to be readable
    meta_data <- meta_data %>% select(-`Time Series Workbook`, -X__2) %>%
      filter(!is.na(X__1)) %>%
      head(max(lengths(.)) - 1) %>%
      tail(max(lengths(.)) - 1) %>%
      `colnames<-`(.[1,]) %>%
      `colnames<-`(str_replace_all(colnames(.), " ", "_")) %>%
      `colnames<-`(str_replace_all(colnames(.), "\\.", "")) %>%
      tail(max(lengths(.)) - 1) %>%
      mutate(Series_Start = as.Date(as.POSIXct(as.numeric(Series_Start) * (60*60*24),
                                               origin = "1899-12-30",
                                               format = "%Y-%m-%d",
                                               tz = "GMT")),
             Series_End = as.Date(as.POSIXct(as.numeric(Series_End) * (60*60*24),
                                             origin = "1899-12-30",
                                             format = "%Y-%m-%d",
                                             tz = "GMT")))
  }
  return(meta_data)

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
