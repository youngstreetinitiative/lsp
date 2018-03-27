#'  Return data frame with the meta data removed
#'
#'  For a typcial sheet 2
#'
#' @name process_sheet_old
#' @param input_ls ABS data frame
#' @return a data frame
#' @export


process_sheet_old <- function(input_ls) {

  #message(input_ls$name)

  # parts from Alex's fun to grab details for cover... ====
  meta_tbl_umbrella <- lsp:::umbrella(input_ls)

  #message("Umbrella Done")

  if (input_ls$table_type == "Index") {

    cat_no <- regmatches(meta_tbl_umbrella[1], gregexpr("[[:digit:]]+", meta_tbl_umbrella[1])) %>%
      unlist() %>%
      str_c(collapse = ".")

    Table_Title <- str_trim(str_replace(meta_tbl_umbrella[2], unlist(ex_between(meta_tbl_umbrella[2], "T", ".", include.markers = TRUE)), ""))

    Table_No <- str_replace(unlist(ex_between(meta_tbl_umbrella[2], "T", ".", include.markers = TRUE)), "\\.", "") %>%
      str_replace_all(c(
        "TABLE" = "",
        "Table" = "",
        "table" = ""
      )) %>% str_trim()

    cat_no <- paste0(cat_no,Table_No)


  } else if (input_ls$table_type == "Multi Table") {

    cat_no <- regmatches(meta_tbl_umbrella[1], gregexpr("[[:digit:]]+", meta_tbl_umbrella[1])) %>%
      unlist() %>%
      str_c(collapse = ".")

    Table_Title <- collapse(unlist(regmatches(meta_tbl_umbrella[1], gregexpr("[[:alpha:]]+", meta_tbl_umbrella[1]))), sep = " ")

  }

  full_name <- glue("ABS_{cat_no}_{Table_Title}")


  #message("Full Name done")

  # my parts =====
  var_meta <- input_ls$file_path %>%
    read_excel(sheet = 1) %>%
    meta_clean(., table_type = input_ls$table_type)

  #message("Meta Table Done")

  sheet_names <- lsp:::sheet_namer(input_ls, var_meta)

  #message(glue("--- has {length(sheet_names)} sheet/s"))

  working_tbl <- lsp:::work_table(input_ls, sheet_names)

  #print(working_tbl)

  #message("Working Table created")

  if (input_ls$table_type == "Index") {
    original_nms <- colnames(working_tbl)
    new_nms <- clean_names_full(original_nms)

    var_meta <- var_meta %>%
      mutate(Data_Item_Description = new_nms[-1])

    new_nms[1] <- "Date"
    colnames(working_tbl) <- new_nms

    working_tbl <- working_tbl %>%
      mutate(Date = as.POSIXct(as.numeric(Date) * (60*60*24), origin = "1899-12-30", format = "%Y-%m-%d", tz = "GMT"))

    split_key <- var_meta %>%
      distinct(Data_Item_Description, Series_Type)

    working_tbl <- working_tbl %>%
      gather(cat, val, 2:length(.)) %>%
      inner_join(split_key, by = c("cat" = "Data_Item_Description")) %>%
      split(.$Series_Type) %>%
      map( ~ spread(.x, cat, val))
  } else if (input_ls$table_type == "Multi Table") {
    working_tbl <- working_tbl

  }

  working_list <- list("var_meta" = var_meta, "working_tbl" = working_tbl, "full_name" = full_name)

  # message("Process Sheets Finished")
  #
  # return(working_list)
}


