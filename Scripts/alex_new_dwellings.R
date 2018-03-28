# This is for creating some summary graphs for dwelling supply in Australia but Sydney and Melbourne are the focus

working_directory <- "/Dropbox (YSI)/YSI Team Folder/Content/Housing Affordability Product/Housing Pitch Work (Immanuel & Alex)/Data/lsp_read"

Bundle_Sheets(DropboxDataFolder = working_directory)
Housing_List <- master_list
rm(master_list)

Approvals <- pluck(Housing_List,
                   "ABS_6416.4&5_Median Price (unstratified) and Number of Transfers (Capital City and Rest of State)_Sep2017",
                   "working_tbl")

approval_value <- pluck(Housing_List,
                        "ABS_8731.38_Value of Building Approved - Australia_Nov2017",
                        "working_tbl")

df_value <- approval_value %>% filter(Series.Type == "Original",
                                      VariableClass2 == "Total.Residential",
                                      VariableClass3 == "New") %>%
  select(Date, Value, VariableClass3) %>%
  `colnames<-`(c("Date", "Value '000", "Residential Type")) %>%
  mutate()

ggplot(data = df_value, aes(x = Date, y = `Value '000`)) +
  geom_line(aes(group = `Residential Type`, colour = `Residential Type`))
