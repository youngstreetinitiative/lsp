#'  Return data frame with the meta data removed
#'
#'  For a typcial sheet 2
#'
#' @name process_sheet
#' @param df ABS data frame
#' @return a data frame
#' @export



process_sheet <- function(input_ls) {

  var_meta <- input_ls$file_path %>%
    read_excel(sheet = 1) %>%
    meta_clean()

  working_tbl <- input_ls$file_path %>%
    read_excel(sheet = 2) %>%
    remove_meta()

  original_nms <- colnames(working_tbl)
  new_nms <- clean_names_full(original_nms)
  var_meta <- var_meta %>%
    mutate(Data_Item_Description = new_nms[-1])

  new_nms[1] <- "Date"
  colnames(working_tbl) <- new_nms

  working_tbl <- working_tbl %>%
    mutate(Date = as.POSIXct(as.numeric(Date) * (60*60*24), origin="1899-12-30", tz="GMT"))

  split_key <- var_meta %>%
    distinct(Data_Item_Description, Series_Type)

  working_tbl <- working_tbl %>%
    gather(cat, val, 2:length(.)) %>%
    inner_join(split_key, by = c("cat" = "Data_Item_Description")) %>%
    split(.$Series_Type) %>%
    map( ~ spread(.x, cat, val))

  list(var_meta = var_meta, working_tbl = working_tbl)

}
