

#'  Return a table with references for data on each table
#'
#' @name GraphSourcesfn
#' @param TableName
#' @param FromMasterList Logical: True if Table is in the master list
#' @param MasterTbl Name of the master_list table
#' @param Source Organisation which is the source of the data set
#' @param Catalogue If there is a catelogue identifier write it in here
#' @param LongTableName Long name of the data table, with spaces or underscores
#' @return Data frame
#' @export

GraphSourcesfn <- function(TableName = NULL, Year = NA, Table_No = NA,
                           MasterList = TRUE, Data_List = master_list, SubTable = NA,
                           Source = NULL, Catalogue = NA, LongTableName = NA) {

  ##

  if(MasterList){
    NewSource <- tibble(Table = TableName,
                        Name = pluck(Data_List,
                                     SubTable,
                                     "full_name"),
                        Year = str_extract(as.character(pluck(Data_List,
                                                 SubTable,
                                                 "Month_Year")),
                                           "[0-9]+"))
    NewSource <- NewSource  %>%
      mutate(Source = str_extract(Name, "[A-Za-z]+"),
             Catalogue = str_extract(Name, "[0-9]+"))

    ## Table number depends on type

    NewSource <- NewSource  %>%
      mutate(Table_No = as.character(str_extract_all(Name, "[0-9]+")[[1]][2]))

    NewSource <- NewSource  %>%
      mutate(Reference = ifelse(Source == "ABS",
                                paste(Source, "Cat.", Catalogue, "Table", Table_No, sep = " "),
                                Source),
             ReferencePrintOut = gsub("_", " ", Name))

  } else {
    NewSource <- tibble(Table = ifelse(is.na(TableName), "", TableName),
                        Table_No = ifelse(is.na(Table_No), "", Table_No),
                        Year = Year,
                        Source = Source,
                        Catalogue = Catalogue)
    NewSource <- NewSource  %>%
      mutate(Name = paste(Source, Catalogue, LongTableName, Year, sep = "_"))

  }

  # Determining reference printout text for graph captions
  NewSource <- NewSource  %>%
    mutate(Reference = ifelse(Source == "ABS",
                              paste(Source, "Cat.", paste(Catalogue, Table_No, sep = "."), sep = " "),
                              paste(Source, Catalogue, Table_No, sep = " ")),
           ReferencePrintOut = gsub("_", " ", Name))


  ## Adding new source to GraphSources table

  if(exists("GraphSources")){
    GraphSources <- bind_rows(GraphSources, NewSource)
  }else{
    GraphSources <- NewSource
  }

  GraphSources <<- GraphSources

}


#'  Caption reference
#'
#' @name RefCaption
#' @param TableNames Loaded tables in GraphSources table
#' @param GraphSources Reference table made by GraphSourcesfn when loading data
#' @return String
#' @export
#'

RefCaption <- function(TableNames = NULL){
  if(length(TableNames) == 1){
  caption <- paste("Source:", gsub("\\\"|c|\\(|\\)","",
                                 paste(GraphSources %>%
                                         filter(Table == TableNames) %>%
                                         distinct %>%
                                         select(Reference))))
  }else{
    caption <- paste("Sources:", gsub("\\\"|c|\\(|\\)","",
                                      paste(GraphSources %>%
                                              filter(Table %in% TableNames) %>%
                                              distinct %>%
                                              select(Reference))))
  }
  return(caption)
}

