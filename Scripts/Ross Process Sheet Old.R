#This is Ross' original process sheets
process_sheet <- function(input_ls) {

  message(input_ls$name)

  # parts from Alex's fun to grab details for cover... ====
  meta_tbl_umbrella <- input_ls$file_path %>%
    read_excel(sheet = 1) %>%
    select(`Time Series Workbook`) %>%
    filter(!is.na(`Time Series Workbook`)) %>%
    pull(1)

  cat_no <- regmatches(meta_tbl_umbrella[1], gregexpr("[[:digit:]]+", meta_tbl_umbrella[1])) %>%
    unlist() %>%
    str_c(collapse = ".")

  Table_Title = str_trim(str_replace(meta_tbl_umbrella[2], unlist(ex_between(meta_tbl_umbrella[2], "T", ".", include.markers = TRUE)), ""))

  Table_No = str_replace(unlist(ex_between(meta_tbl_umbrella[2], "T", ".", include.markers = TRUE)), "\\.", "") %>%
    str_replace_all(c(
      "TABLE" = "",
      "Table" = "",
      "table" = ""
    )) %>% str_trim()

  cat_no <- paste0(cat_no,Table_No)

  full_name <- glue("ABS_{cat_no}_{Table_Title}")


  # my parts =====
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
    mutate(Date = as.POSIXct(as.numeric(Date) * (60*60*24), origin="1899-12-30", format="%Y-%m-%d", tz="GMT"))

  split_key <- var_meta %>%
    distinct(Data_Item_Description, Series_Type)

  working_tbl <- working_tbl %>%
    gather(cat, val, 2:length(.)) %>%
    inner_join(split_key, by = c("cat" = "Data_Item_Description")) %>%
    split(.$Series_Type) %>%
    map( ~ spread(.x, cat, val))




  list(var_meta = var_meta, working_tbl = working_tbl, full_name = full_name)

}

