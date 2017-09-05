#'  Return data frame with the meta data removed
#'
#'  For a typcial sheet 2
#'
#' @name process_sheet
#' @param df ABS data frame
#' @return a data frame
#' @export



process_sheet <- function(input_ls) {

  message(input_ls$name)

  var_meta <- input_ls$file_path %>%
    read_excel(sheet = 1) %>%
    meta_clean()

  sheet_count <- input_ls$file_path %>%
    excel_sheets() %>%
    .[!. %in% c("Index", "Inquiries")]

  message(glue("--- has {length(sheet_count)} sheet/s"))

  if(length(sheet_count) == 1) {
    working_tbl <- input_ls$file_path %>%
      read_excel(sheet = 2) %>%
      remove_meta()
  } else {
    working_tbl <- sheet_count %>%
      seq_along() %>%
      map( ~ read_excel_multi(input_ls$file_path, sheet = .x)) %>%
      bind_cols() %>%
      remove_meta()
  }

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
