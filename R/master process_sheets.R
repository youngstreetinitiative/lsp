#'  Return data frame with the meta data removed
#'
#' @name process_sheets
#' @param input_ls List of ABS file locations with file paths saved as the object 'file_path' (and other meta information) created by the Bundle_Sheets function
#' @param InclVarMeta Logical: include the variable information as columns in working_tbl.
#' @param LookForIndustry Logical: enables the process to look for possible columns and row variables which define the industry for the variable from which it creates another column vector on working_tbl with industry abbreviations as how they conform to ANZIC categories.
#' @return a data frame
#' @export


process_sheets <- function(input_ls, InclVarMeta = FALSE, LookForIndustry = FALSE) {

  # parts from Alex's fun to grab details for cover... ====
  meta_tbl_umbrella <- lsp:::umbrella(input_ls)

  var_meta <- input_ls$file_path %>%
    read_excel(sheet = 1) %>%
    meta_clean(table_type = input_ls$table_type)

  sheet_names <- lsp:::sheet_namer(input_ls, var_meta)

  if (input_ls$table_type == "Index") {

    cat_no <-  gsub("\\..*$", "", meta_tbl_umbrella[1])
    # Take our the number before the first dot

    Table_No <- (meta_tbl_umbrella[2] %>%
                   str_replace(":", ".") %>%
                   ex_between("\\s", ".") %>%
                   unlist() %>%
                   str_replace_all(c("\\s+and\\s" = "&")) %>%
                   str_trim())[1]
    # Extract the numbers after the space (which always follows "Table")

    Table_Title <- meta_tbl_umbrella[2] %>%
      rm_between_multiple(c("Table", "Table", "TABLE", "TABLE"),
                          c(".", ":", ".", ":"),
                          trim = T)

    # Find date of latest data point from meta data page
    Series_End_Date <- var_meta %>%
      summarise(Data.month_test = max(Series_End)) %>%
      pull(1)

    Data.month <- Series_End_Date %>%
      format("%b%Y")

    # Full Name created by joining catalogue number, title and month of release/latest data
    full_name <- glue("ABS_{cat_no}.{Table_No}_{Table_Title}_{Data.month}")

  } else if (input_ls$table_type == "Multi_Table") {

    cat_no <- gsub(" .*$", "", meta_tbl_umbrella[1])
    # Substitute everything after the first space (the space before the dot in gsub) as blank

    Table_Title <- meta_tbl_umbrella[1] %>%
      substring((gregexpr("\\s", meta_tbl_umbrella[1]) %>%
                  unlist())[1]) %>%
      str_trim()
    # Derive the position of the first space with gregexpr, then substring to that character and trim

    Release_Time <- meta_tbl_umbrella[2] %>%
      str_replace_all(c("Released at " = "",
                        " \\(Canberra time\\)" = ""))

    Data.month <- Release_Time %>%
      strptime("%I.%M%p %d %B %Y") %>%
      format("%b%Y")
    # some ABS datasets have a different date format including weekday (%a)
    if(is.na(Data.month)){
      Data.month <- Release_Time %>%
        strptime("%I:%M %p %a %d %b %Y") %>%
        format("%b%Y")
    }

    # Full Name created by joining catalogue number, title, first table name and month of release/latest data
    full_name <- glue("ABS_{cat_no}_{Table_Title}_from {sheet_names[1]}_{Data.month}")

  }


  if (input_ls$table_type == "Index") {

  ## loading in working_table

    if (length(sheet_names) == 1) {
      working_tbl <- input_ls$file_path %>%
        read_excel(sheet = 2,
                   col_names = F)
    } else {

      working_tbl <- sheet_names %>%
        seq_along() %>%
        map( ~ read_excel_multi(input_ls$file_path,
                                sheet = .x)) %>%
        bind_cols()
    }

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

    # For variables with a different length of components
    for (i in 1:length(working_tbl_VarNameSplit)){
      if (length(working_tbl_VarNameSplit[[i]]) < MaxNumVarNames){
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
                                             tz = "GMT")))

    if(InclVarMeta){
      working_tbl <- working_tbl %>%
        select(VarNamesList, Date, Value, VarNames, meta_names[-1])
    }else{
      working_tbl <- working_tbl %>%
        select(VarNamesList, Series.Type, Date, Value)
    }

    working_list <- list("Table_Title" = Table_Title,
                         "full_name" = full_name,
                         "Month_Year" = Data.month,
                         "Series_End_Date" = Series_End_Date,
                         "var_meta" = var_meta,
                         "working_tbl" = working_tbl)

  } else if (input_ls$table_type == "Multi_Table") {

    #
    working_tables <- list()
    ## Loading in each table at a time looping
    for(i in 2:(length(sheet_names) + 1)){

      working_tbl_i <- input_ls$file_path %>%
        read_excel(sheet = i)

      # Save the table name on the sheet
      table_name <- (working_tbl_i %>% pull(1))[which(str_detect(working_tbl_i %>% pull(1), "Table"))]
      # start from first row with an observation in column 2
      wt_startrow <- which(is.na(working_tbl_i %>% pull(1)))[1]
      # cut out the top rows above start row
      working_tbl_i <- working_tbl_i[wt_startrow:nrow(working_tbl_i),]
      # find the first row of the data as the first NA in the second column after cutting
      data_startrow <- which(!is.na(working_tbl_i %>% pull(1)))[1]

      # data frame of column names
      column_names <- working_tbl_i[1:(data_startrow - 1),]

      # Rotate and apply column headings across columns
      column_names <- t(column_names)[-1,] %>%
        as_tibble
      # Find location of NA columns & drop them
      NACols <- c()
      for(j in 1:ncol(column_names)){
        if (all(is.na(column_names[,j]))) {
          NACols <- c(NACols,j)
        }
      }
      column_names[,NACols] <- NULL

      # Apply column headings across
      for(j in 1:ncol(column_names)){
        for(k in 1:nrow(column_names)){
          if(is.na(column_names[k,j]) & k > 1){
            column_names[k,j] <- column_names[k - 1,j]
            }
        }
      }

      # Paste together column headings to get an identifier column
      column_name <- clean_names_full(do.call(paste, c(column_names, sep = "_")))
      column_name_row <- c("RowName1", column_name)

      column_names_df <- cbind(ColumnName = column_name, column_names)
      colnames(column_names_df) <- c("ColName",
                                     c(paste0(rep("ColName"),
                                              seq(1, (ncol(column_names_df) - 1)))))

      # Binding new column names with modified names
      working_tbl_i <- working_tbl_i[data_startrow:nrow(working_tbl_i),]
      colnames(working_tbl_i) <- column_name_row

      # Seperating out row names
      working_tbl_i <- working_tbl_i %>%
        as.tibble %>%
        mutate(RowName2 = NA,
               RowName3 = NA,
               RowName4 = NA,
               RowName1 = clean_names_full(RowName1))

      for(l in 1:length(working_tbl_i$RowName1)){
        # Deriving RowName2 --> row is all NAs and is hence a heading
        if (is.na(working_tbl_i[l, 2])){
          working_tbl_i$RowName2[l] <- working_tbl_i$RowName1[l]
        } else if (l > 1) {
          if (!is.na(working_tbl_i$RowName2[l - 1])){
            # if the RowName2 above is named, then apply it
            working_tbl_i$RowName2[l] <- working_tbl_i$RowName2[l - 1]
          } else {
            working_tbl_i$RowName2[l] <- NA
          }
        } else {
          working_tbl_i$RowName2[l] <- NA
        }

        # Deriving RowName3 --> 1 row on NAs below the heading row
        if (l > 1) {
          if (is.na(working_tbl_i[l, 2]) &
              is.na(working_tbl_i[l + 1, 2])) {
            working_tbl_i$RowName3[l] <- working_tbl_i$RowName1[l]
          } else if (!is.na(working_tbl_i$RowName3[l - 1])) {
            # if the RowName3 above is named, then apply it
            working_tbl_i$RowName3[l] <- working_tbl_i$RowName3[l - 1]
          } else {
            working_tbl_i$RowName3[l] <- NA
          }
        } else {
          working_tbl_i$RowName3[l] <- NA
        }

        # Deriving RowName4 --> 2 rows on NAs below the heading row
        if (l > 2) {
          if (is.na(working_tbl_i[l, 2]) &
              is.na(working_tbl_i[l + 1, 2]) &
              is.na(working_tbl_i[l + 2, 2])) {
            working_tbl_i$RowName4[l] <- working_tbl_i$RowName1[l]
          } else if (!is.na(working_tbl_i$RowName4[l - 1])) {
            # if the RowName4 above is named, then apply it
            working_tbl_i$RowName4[l] <- working_tbl_i$RowName4[l - 1]
          } else {
            working_tbl_i$RowName4[l] <- NA
          }
        } else {
          working_tbl_i$RowName4[l] <- NA
        }
      }

      # filter out rows without data by the second column
      working_tbl_i <- working_tbl_i[!is.na(working_tbl_i[,2]),]

      if(LookForIndustry == T){
        ## Checking for industry variables and creating new industry variable
        if(any(str_detect(RowNameVars, pattern = "Mining"))){
            ## Checking for industry variables and creating new industry variable
            working_tbl_i <- working_tbl_i %>%
              mutate(Industry = IndustryNames[RowName1],
                     Industry = ifelse(is.na(Industry), IndustryNames[RowName2], Industry),
                     Industry = ifelse(is.na(Industry), IndustryNames[RowName3], Industry),
                     Industry = ifelse(is.na(Industry), IndustryNames[RowName4], Industry))
          }
        # Bring columns into a factor
        working_tbl_i <- working_tbl_i %>%
          gather(ColName, Value,
                 -c(RowName1, RowName2, RowName3, RowName4, Industry)) %>%
          mutate(Value = as.numeric(Value))
      } else {
        # Bring columns into a factor
        working_tbl_i <- working_tbl_i %>%
          gather(ColName, Value,
                 -c(RowName1, RowName2, RowName3, RowName4)) %>%
          mutate(Value = as.numeric(Value))
      }

      # Drop RowName variables if they are all NAs
      if(all(is.na(working_tbl_i$RowName2))){
        working_tbl_i <- working_tbl_i %>%
          select(-RowName2)
      }
      if(all(is.na(working_tbl_i$RowName3))){
        working_tbl_i <- working_tbl_i %>%
          select(-RowName3)
      }
      if(all(is.na(working_tbl_i$RowName4))){
        working_tbl_i <- working_tbl_i %>%
          select(-RowName4)
      }

      # Binding back the RowName variables to values
      working_tbl_i <- merge(column_names_df, working_tbl_i, all.x = T) %>%
        select(-ColName)

      # Adding the working table to the working table list for the spreadsheet
      sheet <- i - 1
      s_n <- sheet_names[sheet]
      working_tables[[s_n]] <- working_tbl_i
    }

    # Remove the blank element at the beginning of the working_table list
    #working_tables[[1]] <- NULL
    # Name the tables in order
    #names(working_tables) <- as.vector(sheet_names)
    working_list <- list("Table_Title" = Table_Title,
                         "full_name" = full_name,
                         "Month_Year" = Data.month,
                         "Release_Time" = Release_Time,
                         "var_meta" = var_meta,
                         "working_tables" = working_tables)
  }



  return(working_list)
}
