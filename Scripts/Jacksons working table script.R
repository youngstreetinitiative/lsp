
library(lsp)
IndGVAtest <- read_excel("C:/Users/User/Dropbox (YSI)/YSI Team Folder/Content/Economy/Growth/General/Data/National Accounts/5206045_industry_gva_current_price.xlsx" ,
                         sheet = 2,
                         col_names = F)

working_tbl <- IndGVAtest

working_tbl[1, 1] <- "VarNames"
working_tbl$X__1 <- clean_names_full(working_tbl$X__1)
working_tbl[1, ] <- clean_names_full(working_tbl[1, ])

meta_lower_bound <- which(str_detect(working_tbl %>% pull(1), "Series.ID"))
meta_names <- working_tbl$X__1[1:meta_lower_bound]

working_tbl <- t(working_tbl) %>%
  as.tibble(row.names = NA)

colnames(working_tbl) <- working_tbl[1,]
working_tbl <- working_tbl[-1,]

## Variable names break-up
VarNames <- as.character(working_tbl$VarNames)
MaxNumVarNames <- max(str_count(VarNames, "_")) + 1
working_tbl_VarNameSplit <- strsplit(VarNames, "_")

# For variables with a different length
for (i in 1:length(working_tbl_VarNameSplit)){
  if (length(working_tbl_VarNames[[i]]) < MaxNumVarNames){
    working_tbl_VarNameSplit[[i]] <- c(working_tbl_VarNameSplit[[i]],
                                   rep("", MaxNumVarNames - length(working_tbl_VarNameSplit[[i]])))
  }
}
working_tbl_VarNameSplit <- data.frame(matrix(unlist(working_tbl_VarNameSplit),
                                          ncol = MaxNumVarNames,
                                          byrow = T),
                                   stringsAsFactors = FALSE)
VarNamesList <- c(paste0(rep("VariableClass"), seq(1, MaxNumVarNames)))
colnames(working_tbl_VarNameSplit) <- VarNamesList



## Checking for industry variables and creating new industry variable
if(any(str_detect(working_tbl_VarNameSplit, pattern = "Mining")) == T){
  working_tbl_VarNameSplit$Industry <- NA
  VarNamesList <- c(VarNamesList, "Industry")
  for (i in 1:MaxNumVarNames){
    if(any(str_detect(working_tbl_VarNameSplit[,i], pattern = "Mining")) == T){
      for(j in 1:length(working_tbl_VarNameSplit[,1])){
        if(is.na(working_tbl_VarNameSplit$Industry[j])){
          working_tbl_VarNameSplit$Industry[j] <- IndustryNames[working_tbl_VarNameSplit[j,i]]
        }
      }
    }
  }
}
# Binding variable name splits to working_tbl
working_tbl <- cbind(working_tbl_VarNameSplit, working_tbl)

# Turning dates from columns to rows
working_tbl <- working_tbl %>%
  gather(Date, Value, -c(meta_names, VarNamesList))

# Fixing the date variables
working_tbl <- working_tbl %>%
  mutate(Value = as.numeric(Value),
         Date = as.Date(as.POSIXct(as.numeric(str_extract(Date, "[0-9]+")) * (60*60*24),
                                   origin = "1899-12-30",
                                   format = "%Y-%m-%d",
                                   tz = "GMT")),
         Series.Start = as.Date(as.POSIXct(as.numeric(Series.Start) * (60*60*24),
                                           origin = "1899-12-30",
                                           format = "%Y-%m-%d",
                                           tz = "GMT")),
         Series.End = as.Date(as.POSIXct(as.numeric(Series.End) * (60*60*24),
                                         origin = "1899-12-30",
                                         format = "%Y-%m-%d",
                                         tz = "GMT"))) %>%
  select(VarNamesList, Date, Value, VarNames, meta_names[-1])

