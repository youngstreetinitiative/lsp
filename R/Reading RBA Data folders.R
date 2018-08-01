#' Reading in RBA Spreadsheets
#'
#' @name Bundle_RBA_Sheets
#' @param DropboxDataFolder Address for folder containing RBA file locations with file paths saved as the object 'file_path' (and other meta information) created by the Bundle_RBA_Sheets function
#' @param ListName Names for the output list
#' @param InclVarMeta Logical: include the variable information as columns in working_tbl.
#' @return a list of lists of data frames
#' @export
#'


Bundle_RBA_Sheets <- function(DropboxDataFolder = NULL,
                              ListName = NULL,
                              InclVarMeta = FALSE) {


  if (!is.null(DropboxDataFolder)) {
    paths::DropboxDirFN(DropboxOrigin = DropboxDataFolder)

    files <- list.files(DropboxDir, pattern = ".xls") %>%
      map( ~ list(
        name = gsub(("\\.xlsx|\\.xls"), "", .x),
        file_path =  glue("{DropboxDir}/{.x}"),
        table_type = lsp:::sheet_check(glue("{DropboxDir}/{.x}")))) %>%
      set_names(map_chr(., "name"))

    master_ls <- files %>%
      map( ~ lsp:::process_RBA_sheets(.x,
                                      InclVarMeta = InclVarMeta))

    # Pulling up full_name from each list element and then saving as name of the element
    nms <- master_ls %>%
      map_chr("full_name")
    master_ls <- master_ls %>%
      set_names(nms)

    # Still don't know what this part does
    rdspath <- glue("{DropboxDir}/master_ls.rds")
    write_rds(master_ls, rdspath)
    master_list <<- read_rds(rdspath)
    assign(ListName, master_list, envir = globalenv())
  }

}

#' Processing RBA Spreadsheets into Usable Lists
#'
#' @name process_RBA_sheets
#' @param input_ls List of RBA file locations with file paths saved as the object 'file_path' (and other meta information) created by the Bundle_RBA_Sheets function
#' @param InclVarMeta Logical: include the variable information as columns in working_tbl.
#' @return a data frame
#' @export
#'

process_RBA_sheets <- function(input_ls, InclVarMeta = FALSE) {

  input_ls <- input_ls

  # For time series spreadsheets
  if (input_ls$table_type == "Data") {

  Table_Title <- (input_ls$file_path %>%
    read_excel(sheet = "Data",
               col_names = F))[1,1] %>%
    as.character %>%
    str_replace_all("–", "")

  full_name <- glue("RBA_{Table_Title}")

  working_tbl <- input_ls$file_path %>%
    read_excel(sheet = "Data",
               skip = 1,
               col_names = F)

  NArows <- which(is.na(working_tbl %>% pull(1)))
  working_tbl <- working_tbl[-NArows,]


  meta_lower_bound <- which(str_detect(working_tbl %>% pull(1), "Series ID"))
  meta_names <- working_tbl$X__1[1:meta_lower_bound]

  var_meta <- working_tbl[1:meta_lower_bound,] %>%
    t %>%
    as.tibble(row.names = NA)
  colnames(var_meta) <- var_meta[1,]
  var_meta <- var_meta[-1,]


  working_tbl <- t(working_tbl) %>%
    as.tibble(row.names = NA)

  colnames(working_tbl) <- working_tbl[1,]
  working_tbl <- working_tbl[-1,]




  ## Variable names break-up
  VarNames <- as.character(working_tbl$Title)
  MaxNumVarNames <- max(str_count(as.character(VarNames), "; ")) + 1
  VarnamesSplit <- strsplit(as.character(VarNames), "; ")

  # For variables with a different length of components
  for (i in 1:length(VarnamesSplit)){
    if (length(VarnamesSplit[[i]]) < MaxNumVarNames){
      VarnamesSplit[[i]] <- c(VarnamesSplit[[i]],
                              rep("", MaxNumVarNames - length(VarnamesSplit[[i]])))
    }
  }

  VarnamesSplit <- data.frame(matrix(unlist(VarnamesSplit),
                                     ncol = MaxNumVarNames,
                                     byrow = T),
                              stringsAsFactors = FALSE)
  VarNamesList <- c(paste0(rep("VariableClass"), seq(1, MaxNumVarNames)))
  colnames(VarnamesSplit) <- VarNamesList

  # Binding variable name splits to working_tbl
  # Turning dates from columns to rows
  # Fixing the date variables

  working_tbl <- cbind(working_tbl, VarnamesSplit) %>%
    gather(Date, Value, -c(meta_names, VarNamesList)) %>%
    mutate(Date = as.Date(as.POSIXct(as.numeric(str_extract(Date, "[0-9]+")) * (60*60*24),
                                     origin = "1899-12-30",
                                     format = "%Y-%m-%d",
                                     tz = "GMT")),
           Value = as.numeric(Value))

  if(InclVarMeta){
    working_tbl <- working_tbl %>%
      select(VarNamesList, Date, Value, VarNames, meta_names[-1])
  }else{
    working_tbl <- working_tbl %>%
      select(VarNamesList, Date, Value)
  }

  working_list <- list("Table_Title" = Table_Title,
                       "full_name" = full_name,
                       "var_meta" = var_meta,
                       "working_tbl" = working_tbl)

  } else { # for non-time series spreadsheets

    Table_Title <- as.character((input_ls$file_path %>%
                                   read_excel(sheet = 1,
                                              col_names = F))[1,1])

    full_name <- glue("RBA_{Table_Title}")

    working_tbl <- input_ls$file_path %>%
      read_excel(sheet = 1,
                        skip = 1,
                        col_names = F)

    working_list <- list("Table_Title" = Table_Title,
                         "full_name" = full_name,
                         "working_tbl" = working_tbl)
  }
  return(working_list)
}
