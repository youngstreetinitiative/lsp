# ./R/YSI-palette.R
# Defines font and a YSI palette, and makes them ggplot2 defaults

YSI_font <- "Calibri"

theme_set(theme_classic(base_family = YSI_font))

YSI_palette <- c("#FF6633", "#6699CC", "#666666", "#00004d", "#009900", "#660033", "#33CCCC", "#996600",
                 "#660066", "#00CC99", "#FF3333", "#993333", "#CCCC99", "#9900FF", "#FF6600", "#FF00CC",
                 "#0033CC", "#669966", "#00CC00", "#FF9933")

update_geom_defaults("text", list(family = YSI_font))
scale_colour_discrete <- function(...) scale_colour_manual(..., values = YSI_palette)
scale_fill_discrete <- function(...) scale_fill_manual(..., values = YSI_palette)
