
# how to pull inmore than 1 sheet of data




# testing breaking up tables


df <- ls$`5204055_changes_inventories`$working_tbl$Original


test <- df %>%
  select(-Series_Type) %>%
  gather(Series_ID, val, 2:length(.)) %>%
  separate(Series_ID, c("A","B", "C", "D", "E", "G"), sep = "_", extra = "merge", fill = "right", remove = F)


test$A %>% unique()
test$B %>% unique()
