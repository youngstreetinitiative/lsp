#'  Graph Format templates
#'
#' @name GFormTS
#' @param Facet add desc here
#' @param Yaxis add desc here
#' @param YBreaks add desc here
#' @param yintercept0 add desc here
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

#'  Graph Format template 2
#'
#' @name GplotFormTS
#' @param Facet add desc here
#' @param Yaxis add desc here
#' @param YBreaks add desc here
#' @param yintercept0 add desc here
#' @param XyrBreaks add desc here
#' @return GGplot format
#' @export


GplotFormTS <- function(
  Facet = FALSE,
  Yaxis = "",
  YBreaks = 0,
  yintercept0 = FALSE,
  XyrBreaks = NA
){

    # Graph formats for singular graphs
    GFormTS <- ggplot() +
    labs(x = NULL, y = NULL, fill = NULL, colour = NULL) +
      theme_classic(base_size = 12) +
      theme(strip.background = element_rect(colour = "white"))

    # Standard x and y breaks for facetted and singular graphs
     if(Facet == FALSE){
       if(YBreaks == 0){
         YBreaks <- 6
         }
       GFormTS <- GFormTS +
         scale_x_date(date_breaks = "2 years",
                      date_labels = "%Y")
       }else{
         # Graph formats for facetted graphs
         if(YBreaks == 0){
           YBreaks <- 4
           }
         GFormTS <- GFormTS +
           scale_x_date(date_breaks = "5 years",
                        date_labels = "%Y")
         }

    # If year breaks are specified
    if(!is.na(XyrBreaks)){
      GFormTS <- GFormTS +
        scale_x_date(date_breaks = paste(XyrBreaks, "years"),
                     date_labels = "%Y")
    }

    # Add a line at y = 0
    if(yintercept0){
        GFormTS <- GFormTS +
          geom_hline(yintercept = 0)
        }

    # Set y axis labels
    if(Yaxis == "dollar"){
      GFormTS <- GFormTS +
        scale_y_continuous(breaks = pretty_breaks(YBreaks),
                           labels = dollar,
                           sec.axis = dup_axis(name = NULL),
                           expand = c(0, 0))
    }else if(Yaxis == "percent"){
      GFormTS <- GFormTS +
        scale_y_continuous(breaks = pretty_breaks(YBreaks),
                           labels = percent,
                           sec.axis = dup_axis(name = NULL),
                           expand = c(0, 0))
    }else if(Yaxis == "index"){
      GFormTS <- GFormTS +
        geom_hline(yintercept = 100) +
        scale_y_continuous(breaks = pretty_breaks(YBreaks),
                           sec.axis = dup_axis(name = NULL),
                           expand = c(0, 0))

    }else{
      GFormTS <- GFormTS +
        scale_y_continuous(breaks = pretty_breaks(YBreaks),
                           labels = unit_format(unit = Yaxis, sep = ""),
                           sec.axis = dup_axis(name = NULL),
                           expand = c(0, 0))
    }
  return(GFormTS)
}


#'  Graph Format templates
#'
#' @name FormArrange
#' @param GGOutput add desc here
#' @param InclColGuide add desc here
#' @param InclFillGuide add desc here
#' @param title add desc here
#' @param subtitle add desc here
#' @param Ytitle add desc here
#' @param Ytitle2 add desc here
#' @param caption add desc here
#' @param InclLogo add desc here
#' @return Image output of arranged components of a graph
#' @export


FormArrange <- function(GGOutput = NULL, InclColGuide = FALSE, InclFillGuide = FALSE,
                        title = NULL, subtitle = NULL, Ytitle = NULL, Ytitle2 = NA,
                        caption = NULL, InclLogo = FALSE){

  ## If there is not a specified second Y axis, duplicate the name of the first Y axis
  if(is.na(Ytitle2)){
    Ytitle2Adj <- Ytitle
  } else {
    Ytitle2Adj <- Ytitle2
  }

  ## Removing guides when not specified to include
  if(InclColGuide == FALSE){
  GGOutput <- GGOutput +
    guides(colour = "none")
  }
  if(InclFillGuide == FALSE){
    GGOutput <- GGOutput +
      guides(fill = "none")
  }

  if(InclLogo == FALSE){
    FormArrange <- grid.arrange(

    textGrob(title,
             gp = gpar(fontsize = 13)),
    textGrob(subtitle,
             gp = gpar(fontsize = 10)),
    grobTree(
      textGrob(Ytitle,
             gp = gpar(fontsize = 10),
             x = unit(0, "npc"),
             just = c("left", "bottom")),
      textGrob(Ytitle2Adj,
               gp = gpar(fontsize = 10),
                 x = unit(1, "npc"),
               just = c("right", "bottom"))),
    GGOutput,
    textGrob(""),
    textGrob(caption,
             gp = gpar(fontsize = 9),
             x = unit(0, "npc"),
             just = c("left", "bottom"),
             hjust = 0),
    ncol = 1,
    heights = c(0.08, 0.02, 0.02, 0.8, 0.03, 0.05))

  }else{

    if(!exists("YSILogo")){
      YSILogo <<- readPNG(paste0(UserDir, "/Dropbox (YSI)/YSI Team Folder/PowerPoint templates/YSIGraph.png"))
    }

    FormArrange <- grid.arrange(
      grobTree(
        textGrob(""),
        textGrob(title,
             gp = gpar(fontsize = 13)),
        rasterGrob(YSILogo,
                   x = unit(1, "npc"),
                   height = 1,
                   hjust = 1)),
      textGrob(subtitle,
             gp = gpar(fontsize = 10)),
      grobTree(
      textGrob(Ytitle,
               gp = gpar(fontsize = 10),
               x = unit(0, "npc"),
               just = c("left", "bottom"),
               hjust = 0),
      textGrob(Ytitle2Adj,
               gp = gpar(fontsize = 10),
               x = unit(1, "npc"),
               just = c("right", "bottom"),
               hjust = 1)),
    GGOutput,
    textGrob(""),
    textGrob(caption,
             gp = gpar(fontsize = 9),
             x = unit(0, "npc"),
             just = c("left", "bottom"),
             hjust = 0),
    ncol = 1,
    heights = c(0.08, 0.02, 0.02, 0.8, 0.03, 0.05))
  }

  return(invisible(FormArrange))

}
