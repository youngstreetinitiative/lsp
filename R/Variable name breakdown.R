
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


