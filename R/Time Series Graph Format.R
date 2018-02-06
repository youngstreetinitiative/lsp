#'  Graph Format templates
#'
#' @name GFormTS
#' @param
#' @return GGplot format
#' @export


GFormTS <- function(Facet = FALSE, Yaxis = "", YBreaks = 0, yintercept0 = TRUE){
  ## Loading in graph formats
  GFormTS <- ggplot()+
    labs(x = NULL, y = NULL, fill = NULL, colour = NULL)

  if(Facet == FALSE){
    if(YBreaks == 0){
      YBreaks <- 6
      }

    # Graph formats for singular graphs
    GFormTS <- GFormTS +
      theme_minimal(base_size = 12) +
      theme(plot.caption = element_text(hjust = 0,
                                        size = 10)) +
      scale_x_date(date_breaks = "2 years",
                   date_labels = "%Y")

    if(Yaxis == "dollar"){
      GFormTS <- GFormTS +
        scale_y_continuous(breaks = pretty_breaks(YBreaks),
                           labels = dollar)
      }else if(Yaxis == "percent"){
        GFormTS <- GFormTS +
          scale_y_continuous(breaks = pretty_breaks(YBreaks),
                             labels = percent)
        if(yintercept0){
          GFormTS <- GFormTS +
            geom_hline(yintercept = 0)
        }
        }else if(Yaxis == "index"){
          GFormTS <- GFormTS +
            geom_hline(yintercept = 100)
          }
    }else{
      if(YBreaks == 0){
        YBreaks <- 4
        }
      # Graph formats for facetted graphs

      GFormTS <- GFormTS +
        theme_minimal(base_size = 12) +
        theme(plot.caption = element_text(hjust = 0,
                                          size = 10)) +
        scale_x_date(date_breaks = "5 years",
               date_labels = "%Y")
  if(Yaxis == "dollar"){
    GFormTS <- GFormTS +
      scale_y_continuous(breaks = pretty_breaks(YBreaks),
                     labels = dollar)
    }else if(Yaxis == "percent"){
      GFormTS <- GFormTS +
        scale_y_continuous(breaks = pretty_breaks(YBreaks),
                           labels = percent)
      if(yintercept0){
        GFormTS <- GFormTS +
          geom_hline(yintercept = 0)
      }
      }else if(Yaxis == "index"){
        GFormTS <- GFormTS +
          geom_hline(yintercept = 100) +
          scale_y_continuous(breaks = pretty_breaks(YBreaks))
      }
    }


  # # Attaching YSI logo and sources caption
  # GFormTS <- gtable_add_rows(GFormTS, heights = unit(1, "inch"))
  # GFormTS <- gtable_add_grob(GFormTS, "YSI.png", nrow(GFormTS), 1, nrow(GFormTS), ncol(GFormTS))
  #
  # # Turn off clipping
  # GFormTS <- ggplot_gtable(ggplot_build(GFormTS))
  # GFormTS$layout$clip[GFormTS$layout$name == "panel"] <- "off"


  return(GFormTS)

  }





