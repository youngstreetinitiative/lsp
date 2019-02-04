#'  Graph Format template 2
#'
#' @name GplotFormTS
#' @param YBreaks Number of Y-axis breaks (as a guide for pretty_breaks)
#' @param XyrBreaks Distance between year breaks
#' @param Facet Logical. True if the plot is facetted. Sets the Year breaks to be further apart.
#' @param Index Place a horizontal line at height 100 for index measures
#' @param yintercept0 Place a horizontal line at the zero point
#' @param ExpandToMargins Set distance between data points and axes to zero by setting expand = c(0, 0)
#' @param TextSize Size of axis labels (uses base_size inside theme_classic)
#' @return GGplot format
#' @export
#'


GplotFormTS <- function(YBreaks = NA,
                        XyrBreaks = NA,
                        Facet = FALSE,
                        Index = FALSE,
                        yintercept0 = FALSE,
                        ExpandToMargins = FALSE,
                        TextSize = 16){

    # Graph formats for singular graphs
    GFormTS <- ggplot() +
    labs(x = NULL, y = NULL, fill = NULL, colour = NULL) +
      theme_classic(base_size = TextSize) +
      theme(strip.background = element_rect(colour = "white"))

    # Standard x and y breaks for facetted and singular graphs
     if(Facet == FALSE){
       if(is.na(YBreaks)){
         YBreaks <- 4
         }
       GFormTS <- GFormTS +
         scale_x_date(date_breaks = "3 years",
                      date_labels = "%Y")
       }else{
         # Graph formats for facetted graphs
         if(is.na(YBreaks)){
           YBreaks <- 3
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

    # Add a line at 100 for index
    if(Index){
      GFormTS <- GFormTS +
        geom_hline(yintercept = 100)
    }

    # Use the expand option to extend the data to the margins
    if(ExpandToMargins){
      GFormTS <- GFormTS +
        scale_y_continuous(breaks = pretty_breaks(YBreaks),
                           label = comma,
                           expand = c(0, 0))
    }else{
      GFormTS <- GFormTS +
        scale_y_continuous(label = comma,
                           breaks = pretty_breaks(YBreaks))
    }

  return(GFormTS)
}


#'  Graph Format templates
#'
#' @name FormArrange
#' @param GGOutput ggplot object
#' @param Plotly Logical. If true FormArrange sxports the plot as an interactive plot using the ploty function ggplotly rather than a grob object.
#' @param InclColGuide Logical. Include the colour guide for line and point plots
#' @param InclFillGuide Logical. Include the colour guide for bar and histogram plots
#' @param title Plot title sitting top and centre of glob output
#' @param subtitle Plot subtitle in smaller font below the plot title
#' @param Ytitle Y-axis title placed above the axis line and duplicated on both left and right
#' @param Ytitle2 Second Y-axis title for the right side, if it is different from the left side
#' @param caption Text for the plot caption placed below the GGOutput plot
#' @param InclLogo Logical. Include the YSI logo in the top right corner of the output
#' @param TitleSize Font size of title text. Default 15.
#' @param TextSize Font size of y axis titles and subtitle. Default 12.
#' @param CaptionSize Font size of caption. Default 8.
#' @param SaveName Name of file to save to the 'figures' folder. Will only save if there is a string input.
#' @return Image output of arranged components of a graph
#' @export
#'


FormArrange <- function(GGOutput = NULL,
                        Plotly = F,
                        InclColGuide = FALSE,
                        InclFillGuide = FALSE,
                        title = NULL,
                        subtitle = NULL,
                        Ytitle = NULL,
                        Ytitle2 = "",
                        caption = NULL,
                        InclLogo = FALSE,
                        TitleSize = 15,
                        TextSize = 12,
                        CaptionSize = 8,
                        SaveName = NA){

  ## Removing guides when not specified to include
  if(InclColGuide == FALSE){
  GGOutput <- GGOutput +
    guides(colour = "none")
  }
  if(InclFillGuide == FALSE){
    GGOutput <- GGOutput +
      guides(fill = "none")
  }

  if(Plotly == F){

  # if(InclLogo == FALSE){
    FormArrange <- grid.arrange(

    textGrob(title,
             gp = gpar(fontsize = TitleSize)),
    textGrob(subtitle,
             gp = gpar(fontsize = TextSize)),
    grobTree(
      textGrob(Ytitle,
             gp = gpar(fontsize = TextSize),
             x = unit(0, "npc"),
             just = c("left", "bottom")),
      textGrob(Ytitle2,
               gp = gpar(fontsize = TextSize),
                 x = unit(1, "npc"),
               just = c("right", "bottom"))),
    GGOutput,
    textGrob(""),
    textGrob(caption,
             gp = gpar(fontsize = CaptionSize),
             x = unit(0, "npc"),
             just = c("left", "bottom"),
             hjust = 0),
    ncol = 1,
    heights = c(0.1, 0.02, 0.1, 0.7, 0.03, 0.05))

  # }else{
  #
  #   if(!exists("YSILogo")){
  #     YSILogo <<- readPNG(paste0(UserDir, "/Dropbox (YSI)/YSI Team Folder/PowerPoint templates/YSIGraph.png"))
  #   }
  #
  #   FormArrange <- grid.arrange(
  #     grobTree(
  #       textGrob(""),
  #       textGrob(title,
  #            gp = gpar(fontsize = TitleSize)),
  #       rasterGrob(YSILogo,
  #                  x = unit(1, "npc"),
  #                  height = 1,
  #                  hjust = 1)),
  #     textGrob(subtitle,
  #            gp = gpar(fontsize = TextSize)),
  #     grobTree(
  #     textGrob(Ytitle,
  #              gp = gpar(fontsize = TextSize),
  #              x = unit(0, "npc"),
  #              just = c("left", "bottom"),
  #              hjust = 0),
  #     textGrob(Ytitle2,
  #              gp = gpar(fontsize = TextSize),
  #              x = unit(1, "npc"),
  #              just = c("right", "bottom"),
  #              hjust = 1)),
  #   GGOutput,
  #   textGrob(""),
  #   textGrob(caption,
  #            gp = gpar(fontsize = CaptionSize),
  #            x = unit(0, "npc"),
  #            just = c("left", "bottom"),
  #            hjust = 0),
  #   ncol = 1,
  #   heights = c(0.08, 0.02, 0.02, 0.8, 0.03, 0.05))
  # }

  } else {
    GGOutput <- GGOutput +
      labs(title = title,
           subtitle = subtitle,
           y = Ytitle,
           caption = caption)

    FormArrange <- ggplotly(GGOutput)
  }

  if(exists("Saveallfigures")){
    if(Saveallfigures == TRUE){

      if(!exists("FigNum")){
        FigNum <<- 1
      }else{
        FigNum <<- FigNum + 1
      }

      if(exists("Chapter")){
        ggsave(paste0("figures/fig", Chapter, "_", FigNum, ".jpeg"), FormArrange)
      }else{
        message("Missing chapter number")
      }
    }
  }

    if(!is.na(SaveName)){
      ggsave(paste0("figures/", SaveName, ".jpeg"), FormArrange)
    }


  return(invisible(FormArrange))

}
