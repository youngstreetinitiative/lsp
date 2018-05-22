

#'  Return a table with references for data on each table
#'
#' @name GraphSourcesfn
#' @param TableName
#' @param Data_List Name of the master_list table
#' @param MasterList Logical: True if Table is in the master list
#' @param SubTable Table withing Data_List working table is pulled from

#' @param Source Organisation which is the source of the data set
#' @param Catalogue If there is a catelogue identifier write it in here
#' @param LongTableName Long name of the data table, with spaces or underscores
#' @return Data frame
#' @export

GraphSourcesfn <- function(TableName = NULL, Data_List = master_list, SubTable = NA,
                           Year = NA, Table_No = NA, MasterList = TRUE,
                           Source = NULL, Catalogue = NA, LongTableName = NA) {

  ##

  if(MasterList){
    NewSource <- tibble(Table = TableName,
                        Name = pluck(Data_List,
                                     SubTable,
                                     "full_name"))
    NewSource <- NewSource  %>%
      mutate(Source = str_extract(Name, "[A-Za-z]+"),
             Catalogue = str_extract(Name, "[0-9]+"),
             Year = ifelse(Source == "ABS",
                                      str_extract(as.character(pluck(Data_List,
                                                 SubTable,
                                                 "Month_Year")),
                                           "[0-9]+"),
                                      as.character(Year)))

    ## Table number depends on type

    NewSource <- NewSource  %>%
      mutate(Table_No = ifelse(Source == "ABS",
                               as.character(str_extract_all(Name, "[0-9]+")[[1]][2]),
                               ifelse(Source == "RBA",
                                      as.character(str_extract_all(SubTable, "[0-9a-zA-Z]+")[[1]][2]),
                                      ifelse(!is.na(Table_No), Table_No, ""))))

  } else {
    NewSource <- tibble(Table = ifelse(is.na(TableName), "", TableName),
                        Year = as.character(Year),
                        Source = Source,
                        Catalogue = ifelse(is.na(Catalogue), "", Catalogue),
                        Table_No = ifelse(is.na(Table_No), "", Table_No),
                        LongTableName = ifelse(is.na(LongTableName), "", LongTableName))
    NewSource <- NewSource  %>%
      mutate(Name = paste(Source, Year, LongTableName, Catalogue, Table_No, sep = "_"))

  }

  # Determining reference printout text for graph captions
  NewSource <- NewSource  %>%
    mutate(Reference = ifelse(Source == "ABS",
                              paste(Source, "Cat.", Catalogue, "Table", Table_No, sep = " "),
                              ifelse(Source == "RBA",
                                     paste(Source, Table_No, sep = " "),
                                     ifelse(!is.na(Catalogue) & !is.na(Table_No),
                                            paste(Source, Catalogue, Table_No, sep = " "),
                                            Source))),
           ReferencePrintOut = gsub("_", " ", Name))


  ## Adding new source to GraphSources table

  if(exists("GraphSources")){
    GraphSources <- bind_rows(GraphSources %>%
                                  filter(Table != NewSource$Table),
                              # Only copies rows without the same Table name to ensure no double up
                              NewSource)
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

