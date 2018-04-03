## Outmoded functions to keep operational for old scripts

#' @export

bundle_sheets <- function(DropboxDataFolder = NULL) {


  if (!is.null(DropboxDataFolder)) {
    paths::DropboxDirFN(DropboxOrigin = DropboxDataFolder)


    files <- list.files(DropboxDir, pattern = ".xls") %>%
      map( ~ list(
        name = gsub(("\\.xlsx|\\.xls"), "", .x),
        file_path =  glue("{DropboxDir}/{.x}"),
        table_type = lsp:::sheet_check(glue("{DropboxDir}/{.x}")))) %>%
      set_names(map_chr(., "name"))

    master_ls <- files %>%
      map( ~ lsp:::process_sheet_old(.x))

    nms <- master_ls %>% map_chr("full_name")

    master_ls <- master_ls %>%
      set_names(nms)

    rdspath <- glue("{DropboxDir}/master_ls.rds")

    write_rds(master_ls, rdspath)

    master_list <<- read_rds(rdspath)

  } else {
    files <- list.files("data-raw") %>%
      map( ~ list(
        name = gsub(("\\.xlsx|\\.xls"), "", .x),
        file_path =  glue("data-raw/{.x}"),
        table_type = lsp:::sheet_check(glue("data-raw/{.x}")))) %>%
      set_names(map_chr(., "name"))
    master_ls <- files %>%
      map( ~ lsp:::process_sheet_old(.x))

    nms <- master_ls %>% map_chr("full_name")

    master_ls <- master_ls %>%
      set_names(nms)
    write_rds(master_ls, "data/master_ls.rds")
    master_list <<- read_rds("data/master_ls.rds")
  }
}


#'  Return data frame with the meta data removed
#'
#'  For a typcial sheet 2
#'
#' @name process_sheet_old
#' @param input_ls ABS data frame
#' @return a data frame
#' @export


process_sheet_old <- function(input_ls) {

  #message(input_ls$name)

  # parts from Alex's fun to grab details for cover... ====
  meta_tbl_umbrella <- lsp:::umbrella(input_ls)

  #message("Umbrella Done")

  if (input_ls$table_type == "Index") {

    cat_no <- regmatches(meta_tbl_umbrella[1], gregexpr("[[:digit:]]+", meta_tbl_umbrella[1])) %>%
      unlist() %>%
      str_c(collapse = ".")

    Table_Title <- str_trim(str_replace(meta_tbl_umbrella[2], unlist(ex_between(meta_tbl_umbrella[2], "T", ".", include.markers = TRUE)), ""))

    Table_No <- str_replace(unlist(ex_between(meta_tbl_umbrella[2], "T", ".", include.markers = TRUE)), "\\.", "") %>%
      str_replace_all(c(
        "TABLE" = "",
        "Table" = "",
        "table" = ""
      )) %>% str_trim()

    cat_no <- paste0(cat_no,Table_No)


  } else if (input_ls$table_type == "Multi Table") {

    cat_no <- regmatches(meta_tbl_umbrella[1], gregexpr("[[:digit:]]+", meta_tbl_umbrella[1])) %>%
      unlist() %>%
      str_c(collapse = ".")

    Table_Title <- collapse(unlist(regmatches(meta_tbl_umbrella[1], gregexpr("[[:alpha:]]+", meta_tbl_umbrella[1]))), sep = " ")

  }

  full_name <- glue("ABS_{cat_no}_{Table_Title}")


  #message("Full Name done")

  # my parts =====
  var_meta <- input_ls$file_path %>%
    read_excel(sheet = 1) %>%
    meta_clean(., table_type = input_ls$table_type)

  #message("Meta Table Done")

  sheet_names <- lsp:::sheet_namer(input_ls, var_meta)

  #message(glue("--- has {length(sheet_names)} sheet/s"))

  working_tbl <- lsp:::work_table(input_ls, sheet_names)

  #print(working_tbl)

  #message("Working Table created")

  if (input_ls$table_type == "Index") {
    original_nms <- colnames(working_tbl)
    new_nms <- clean_names_full(original_nms)

    var_meta <- var_meta %>%
      mutate(Data_Item_Description = new_nms[-1])

    new_nms[1] <- "Date"
    colnames(working_tbl) <- new_nms

    working_tbl <- working_tbl %>%
      mutate(Date = as.POSIXct(as.numeric(Date) * (60*60*24), origin = "1899-12-30", format = "%Y-%m-%d", tz = "GMT"))

    split_key <- var_meta %>%
      distinct(Data_Item_Description, Series_Type)

    working_tbl <- working_tbl %>%
      gather(cat, val, 2:length(.)) %>%
      inner_join(split_key, by = c("cat" = "Data_Item_Description")) %>%
      split(.$Series_Type) %>%
      map( ~ spread(.x, cat, val))
  } else if (input_ls$table_type == "Multi Table") {
    working_tbl <- working_tbl

  }

  working_list <- list("var_meta" = var_meta, "working_tbl" = working_tbl, "full_name" = full_name)

  # message("Process Sheets Finished")
  #
  # return(working_list)
}


#' Variable name breakdown
#'
#' @name VarNameBreakdownfn
#' @param datatable Output from bundle_sheets
#' @return A gathered dataframe with variable names now split by underscore between VariableClass columns
#' @export
#'


VarNameBreakdownfn <- function(datatable) {

  #
  Varnames <- colnames(datatable)[-c(1:2)]

  Varnames1 <- strsplit(Varnames, "_")

  MaxNumVarNames <- max(str_count(Varnames, "_")) + 1

  for (i in 1:length(Varnames1) ) {
    if (length(Varnames1[[i]]) < MaxNumVarNames) {
      Varnames1[[i]] <- c(Varnames1[[i]], rep("", MaxNumVarNames - length(Varnames1[[i]])))
    }
  }

  Varnames2 <-  data.frame(matrix(unlist(Varnames1),
                                  ncol = MaxNumVarNames,
                                  byrow = T),
                           stringsAsFactors = FALSE)

  colnames(Varnames2) <- c(paste0(rep("VariableClass"), seq(1, MaxNumVarNames)))

  VarnamesTable <- cbind(Varnames, Varnames2)

  datatable <- datatable %>%
    gather(Varnames, Values, -c(Date, Series_Type)) %>%
    mutate(Values = as.numeric(Values))

  tdatatable <- merge(datatable, VarnamesTable) %>%
    select(-Varnames)

  return(tdatatable)
}



# abs_read <- function(path = NULL) {
#   if (!is.null(path)) {
#     paths::DropboxDirFN(DropboxOrigin = path)
#     print(DropboxDir)
#     files <- list.files(DropboxDir) %>%
#       map( ~ list(
#         name = gsub(("\\.xlsx|\\.xls"), "", .x),
#         file_path = glue("{DropboxDir}/{.x}"),
#         table_type = lsp:::sheet_check(glue("{DropboxDir}/{.x}")))) %>%
#       set_names(map_chr(., "name"))
#   } else {
#     files <- list.files("data-raw") %>%
#       map( ~ list(
#         name = gsub(("\\.xlsx|\\.xls"), "", .x),
#         file_path =  glue("data-raw/{.x}"),
#         table_type = lsp:::sheet_check(glue("data-raw/{.x}")))) %>%
#       set_names(map_chr(., "name"))
#   }
#   return(files)
# }


work_table <- function(master_list = NULL, sheet_names = NULL) {

  if (master_list$table_type == "Index") {
    if (length(sheet_names) == 1) {
      working_tbl <- master_list$file_path %>%
        read_excel(sheet = 2) %>%
        remove_meta()
    } else {
      working_tbl <- sheet_names %>%
        seq_along() %>%
        map( ~ read_excel_multi(master_list$file_path, sheet = .x)) %>%
        bind_cols() %>%
        remove_meta()
    }
  } else if (master_list$table_type == "Multi_Table") {

    if (length(sheet_names) == 1) {
      working_tbl <- master_list$file_path %>%
        read_excel(sheet = 2)
    } else {
      working_tbl <- sheet_names %>%
        seq_along() %>%
        map( ~ read_excel(master_list$file_path, sheet = .x))
    }
  }
}


#'  Return data frame with the meta data removed
#'
#'  For a typcial sheet 2
#'
#' @name remove_meta
#' @param df ABS data frame
#' @return a data frame
#' @export
#'
remove_meta <- function(df) {

  meta_lower_bound <- which(str_detect(df %>% pull(1), "Series ID")) + 1
  df[meta_lower_bound:max(lengths(df)),]
}

