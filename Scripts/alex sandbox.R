#This is a sandbox for updating lsp to deal with other kinds of ABS sheets

#first I need to set up path to the folder that I want to read the practice to

p <- "Dropbox (YSI)/YSI Team Folder/Content/Economy/Tax/General Tax/Data/R Test Data"


test <- lsp:::abs_read(path = p)

input_ls <- test[[3]]

test_umbrella_info <- lsp:::umbrella(input_ls)

test_meta_table <- input_ls$file_path %>%
  read_excel(sheet = 1) %>%
  lsp::meta_clean(table_type = input_ls$table_type)


if (input_ls$table_type == "Index") {

  cat_no <- regmatches(test_umbrella_info[1], gregexpr("[[:digit:]]+", test_umbrella_info[1])) %>%
    unlist() %>%
    str_c(collapse = ".")

  Table_Title <- str_trim(str_replace(test_umbrella_info[2], unlist(ex_between(test_umbrella_info[2], "T", ".", include.markers = TRUE)), ""))

  Table_No <- str_replace(unlist(ex_between(test_umbrella_info[2], "T", ".", include.markers = TRUE)), "\\.", "") %>%
    str_replace_all(c(
      "TABLE" = "",
      "Table" = "",
      "table" = ""
    )) %>% str_trim()

  cat_no <- paste0(cat_no,Table_No)


} else if (input_ls$table_type == "Multi Table") {

  cat_no <- regmatches(test_umbrella_info[1], gregexpr("[[:digit:]]+", test_umbrella_info[1])) %>%
    unlist() %>%
    str_c(collapse = ".")

  Table_Title <- collapse(unlist(regmatches(test_umbrella_info[1], gregexpr("[[:alpha:]]+", test_umbrella_info[1]))), sep = " ")

}


full_name <- glue("ABS_{cat_no}_{Table_Title}")

var_meta <- input_ls$file_path %>%
  read_excel(sheet = 1) %>%
  meta_clean(., table_type = input_ls$table_type)

sheet_count <- lsp:::sheet_counter(input_ls, var_meta)

working_tbl <- lsp:::work_table(input_ls, sheet_count)


working_list <- process_sheet_new(input_ls)

working_list <- process_sheet(input_ls)

master_ls <- test %>%
  map( ~ lsp:::process_sheet_new(.x))

nms <- master_ls %>% map_chr("full_name")

master_ls <- master_ls %>%
  set_names(nms)

rdspath <- glue
