

# ross's sandbox
df_sheet_2 <-read_excel("sheets/5204055_changes_inventories.xls", sheet = 2)
df_front_meta <-read_excel("sheets/5204055_changes_inventories.xls", sheet = 1)


df_sheet_2 <-read_excel("sheets/5206006_industry_gva.xls", sheet = 2)
df_front_meta <-read_excel("sheets/5206006_industry_gva.xls", sheet = 1)

files <- list.files("sheets") %>%
  set_names(str_replace(., ".xls", ""))


# making a function to bundle sheet bits into a list

# which row is the series id


meta_lower_bound <-  which(str_detect(df$X__1, "Series ID")) + 1
df_data <- df[meta_lower_bound:max(lengths(df)),]

original_nms <- colnames(df)


df_data <- df[meta_lower_bound:max(lengths(df)),]



# naming the file
glue("ABS_{Sys.Date()}_num_title")



# get series title, number, names...
vec <- df_front_meta %>%
  select(`Time Series Workbook`) %>%
  filter(!is.na(`Time Series Workbook`))



