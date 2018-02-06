

library(lsp)


BusinessIndTest <- read_excel("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Growth/Productivity/Data/81550do001_201516 Business Indicators by industry.xlsx" ,
                          sheet = 4,
                          col_names = F)

BusinessIndTestContents <- read_excel("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Growth/Productivity/Data/81550do001_201516 Business Indicators by industry.xlsx" ,
                              sheet = 1)

LookForIndustry <- FALSE



working_tbl_i <- BusinessIndTest

meta_tbl_umbrella <- BusinessIndTestContents %>%
  select(`Australian Bureau of Statistics`) %>%
  filter(!is.na(`Australian Bureau of Statistics`)) %>%
  pull(1)

cat_no <- regmatches(meta_tbl_umbrella[1],
                     gregexpr("[[:digit:]]+", meta_tbl_umbrella[1])) %>%
  unlist() %>%
  str_c(collapse = ".")

Table_Title <- collapse(unlist(regmatches(meta_tbl_umbrella[1],
                                          gregexpr("[[:alpha:]]+", meta_tbl_umbrella[1]))), sep = " ")


working_tables <- list()

IndProdTest <- read_excel("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Growth/Productivity/Data/For R Analysis TEST/5260 Productivity by industry.xlsx" ,
                         sheet = 7)

working_tbl_i <- IndProdTest
# start from first row with an observation in column 2
wt_startrow <- which(str_detect(working_tbl_i %>% pull(1), "Table"))[1] + 1
wt_endrow <- nrow(working_tbl_i)
# cut out the top rows above start row
working_tbl_i <- working_tbl_i[-(1:wt_startrow),]
# find the first row of the data as the first NA in the second column after cutting
data_startrow <- which(!is.na(working_tbl_i %>% pull(1)))[1]

# data frame of column names
column_names <- working_tbl_i[1:(data_startrow - 1),]

# Rotate and apply column headings across columns & Drop a row if it is all NAs
column_names <- t(column_names)[-1,] %>%
  as_tibble
for(j in 1:ncol(column_names)){
  if (all(is.na(column_names[,j]))) {
    print("NAs present")
    column_names[,j] <- NULL
  }
}

for(j in 1:ncol(column_names)){
  for(k in 1:nrow(column_names)){
    if(is.na(column_names[k,j]) & k > 1){
      column_names[k,j] <- column_names[k - 1,j]
    }
  }
}

# Paste together column headings to get an identifier column
column_name <- clean_names_full(do.call(paste, c(column_names, sep = "_")))
column_name_row <- c("RowNames", column_name)

column_names_df <- cbind(ColumnName = column_name, column_names)
colnames(column_names_df) <- c("ColName",
                               c(paste0(rep("ColName"),
                                        seq(1, (ncol(column_names_df) - 1)))))

# Binding new column names with modified names
working_tbl_i <- working_tbl_i[data_startrow:length(working_tbl_i$X__1),]
colnames(working_tbl_i) <- column_name_row

# Seperating out row names
working_tbl_i <- working_tbl_i %>%
  as.tibble %>%
  mutate(RowCategory1 = NA,
         RowCategory2 = NA,
         RowCategory3 = NA,
         RowNames = clean_names_full(RowNames))

for(i in 1:length(working_tbl_i$RowNames)){
 # Deriving RowCategory1 --> row is all NAs and is hence a heading
  if (is.na(working_tbl_i[i, 2])){
    working_tbl_i$RowCategory1[i] <- working_tbl_i$RowNames[i]
  } else if (i > 1) {
    if (!is.na(working_tbl_i$RowCategory1[i - 1])){
      # if the RowCategory1 above is named, then apply it
      working_tbl_i$RowCategory1[i] <- working_tbl_i$RowCategory1[i - 1]
    } else {
      working_tbl_i$RowCategory1[i] <- NA
    }
  } else {
    working_tbl_i$RowCategory1[i] <- NA
  }

  # Deriving RowCategory2 --> 1 row on NAs below the heading row
  if (i > 1) {
  if (is.na(working_tbl_i[i, 2]) &
     is.na(working_tbl_i[i + 1, 2])) {
    working_tbl_i$RowCategory2[i] <- working_tbl_i$RowNames[i]
  } else if (!is.na(working_tbl_i$RowCategory2[i - 1])) {
    # if the RowCategory2 above is named, then apply it
    working_tbl_i$RowCategory2[i] <- working_tbl_i$RowCategory2[i - 1]
  } else {
    working_tbl_i$RowCategory2[i] <- NA
  }
  } else {
    working_tbl_i$RowCategory2[i] <- NA
  }

  # Deriving RowCategory3 --> 2 rows on NAs below the heading row
  if (i > 2) {
  if (is.na(working_tbl_i[i, 2]) &
     is.na(working_tbl_i[i + 1, 2]) &
     is.na(working_tbl_i[i + 2, 2])) {
    working_tbl_i$RowCategory3[i] <- working_tbl_i$RowNames[i]
  } else if (!is.na(working_tbl_i$RowCategory3[i - 1])) {
    # if the RowCategory3 above is named, then apply it
    working_tbl_i$RowCategory3[i] <- working_tbl_i$RowCategory3[i - 1]
  } else {
    working_tbl_i$RowCategory3[i] <- NA
  }
  } else {
    working_tbl_i$RowCategory3[i] <- NA
  }
}

# filter out rows without data by the second column
working_tbl_i <- working_tbl_i[!is.na(working_tbl_i[,2]),]

if(LookForIndustry == T){
  ## Checking for industry variables and creating new industry variable
  if(any(str_detect(working_tbl_i, pattern = "Mining"))){
    ## Checking for industry variables and creating new industry variable
    working_tbl_i <- working_tbl_i %>%
      mutate(Industry = IndustryNames[RowNames],
             Industry = ifelse(is.na(Industry), IndustryNames[RowCategory1], Industry),
             Industry = ifelse(is.na(Industry), IndustryNames[RowCategory2], Industry),
             Industry = ifelse(is.na(Industry), IndustryNames[RowCategory3], Industry))
  }
  # Bring columns into a factor
  working_tbl_i <- working_tbl_i %>%
    gather(ColName, Values,
           -c(RowNames, RowCategory1, RowCategory2, RowCategory3, Industry)) %>%
    mutate(Values = as.numeric(Values))
} else {
  # Bring columns into a factor
  working_tbl_i <- working_tbl_i %>%
    gather(ColName, Values,
           -c(RowNames, RowCategory1, RowCategory2, RowCategory3)) %>%
    mutate(Values = as.numeric(Values))
}

# Drop RowCategory variables if they are all NAs
if(all(is.na(working_tbl_i$RowCategory1))){
  working_tbl_i <- working_tbl_i %>%
    select(-RowCategory1)
}
if(all(is.na(working_tbl_i$RowCategory2))){
  working_tbl_i <- working_tbl_i %>%
    select(-RowCategory2)
}
if(all(is.na(working_tbl_i$RowCategory3))){
  working_tbl_i <- working_tbl_i %>%
    select(-RowCategory3)
}

# Binding back the RowCategory variables to values
working_tbl_i <- merge(working_tbl_i, column_names_df, all.x = T)


s_n <- sheet_names[i - 1]
working_tables[["Test table"]] <- working_tbl_i

working_list <- list(working_tables)

master_list <- list(working_list)
