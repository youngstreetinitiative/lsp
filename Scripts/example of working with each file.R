files <- list.files("examples") %>%
  map( ~ list(
    name = str_replace(.x, ".xls", ""),
    file_path =  glue("examples/{.x}")
  )) %>%
  set_names(map_chr(., "name"))

# do to each!

input_ls <- files[[1]]


var_meta <- input_ls$file_path %>%
  read_excel(sheet = 1) %>%
  meta_clean()

working_tbl <- input_ls$file_path %>%
  read_excel(sheet = 2) %>%
  remove_meta()

original_nms <- colnames(df_data)
new_nms <- clean_names_full(original_nms)
var_meta <- var_meta %>%
  mutate(Data_Item_Description = new_nms[-1])

new_nms[1] <- "Date"
colnames(working_tbl) <- new_nms

working_tbl <- working_tbl %>%
  mutate(Date = as.POSIXct(as.numeric(Date) * (60*60*24), origin="1899-12-30", tz="GMT"))


# get names
