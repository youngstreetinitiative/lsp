

Bundle_RBA_Sheets(DropboxDataFolder = "Dropbox (YSI)/YSI Team Folder/Content/RBA Datasets/Interest Rates")


Bundle_RBA_Sheets(DropboxDataFolder = "Dropbox (YSI)/YSI Team Folder/Content/RBA Datasets/Household and Business Finances")


files <- list(file_path = "C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/RBA Datasets/Interest Rates/f01hist Interest Rates and Yields - Money Market Monthly.xls",
              table)

DropboxDataFolder <- "Dropbox (YSI)/YSI Team Folder/Content/RBA Datasets/Interest Rates"
InclVarMeta <- F

paths::DropboxDirFN(DropboxOrigin = DropboxDataFolder)

files <- list.files(DropboxDir, pattern = ".xls") %>%
  map( ~ list(name = gsub(("\\.xlsx|\\.xls"), "", .x),
              file_path =  glue("{DropboxDir}/{.x}"),
              table_type = lsp:::sheet_check(glue("{DropboxDir}/{.x}")))) %>%
  set_names(map_chr(., "name"))

master_ls1 <- files %>%
  map( ~ lsp:::process_RBA_sheets(.x,
                                  InclVarMeta = InclVarMeta))


Table_Title <- (files[[1]]$file_path %>%
                  read_excel(sheet = "Data",
                             col_names = F))[1,1] %>%
  as.character %>%
  str_replace_all("–", "")

# Pulling up full_name from each list element and then saving as name of the element
nms <- master_ls %>%
  map_chr("full_name")
master_ls <- master_ls %>%
  set_names(nms)

# Still don't know what this part does
rdspath <- glue("{DropboxDir}/master_ls.rds")
write_rds(master_ls, rdspath)
master_list <<- read_rds(rdspath)






files$`e01hist Household and Business Balance Sheets`$file_path
