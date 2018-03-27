DropboxDataFolder = "Dropbox (YSI)/YSI Team Folder/Content/Economy/Growth/Productivity/Data/For R Analysis TEST"

paths::DropboxDirFN(DropboxOrigin = DropboxDataFolder)


files <- list.files(DropboxDir, pattern = ".xls") %>%
  map( ~ list(
    name = gsub(("\\.xlsx|\\.xls"), "", .x),
    file_path =  as.character(glue("{DropboxDir}/{.x}")),
    table_type = lsp:::sheet_check(glue("{DropboxDir}/{.x}")))) %>%
  set_names(map_chr(., "name"))

working_file <- files[[2]]

meta_tbl_umbrella <- working_file$file_path %>%
  read_excel(sheet = 1) %>%
  select(`Australian Bureau of Statistics`) %>%
  filter(!is.na(`Australian Bureau of Statistics`)) %>%
  pull(1)


cat_no <- regmatches(meta_tbl_umbrella[1],
                     gregexpr("[[:digit:]]+",
                              substr(meta_tbl_umbrella[1],1,10))) %>%
  unlist() %>%
  str_c(collapse = ".")

Table_Title <- collapse(unlist(regmatches(meta_tbl_umbrella[1],
                                          gregexpr("[[:alpha:]]+", meta_tbl_umbrella[1]))), sep = " ")

Release_Time <- str_replace_all(meta_tbl_umbrella[2], c("Released at " = "",
                                                        " \\(Canberra time\\)" = ""))

var_meta <- working_file$file_path %>%
  read_excel(sheet = 1) %>%
  meta_clean(table_type = working_file$table_type)

var_meta <- var_meta %>% select(-`Australian Bureau of Statistics`) %>%
  filter(!is.na(X__1), !is.na(X__2)) %>%
  `colnames<-`(c("Table", "Content"))

sheet_names <- working_file$file_path %>%
  excel_sheets() %>%
  .[!. %in% "Contents"]








