# ./R/YSI-palette.R
# Defines font and a YSI palette, and makes them ggplot2 defaults

YSI_font <- "Calibri"

theme_set(theme_minimal(base_family = YSI_font))

YSI_palette <- c("#800000", "#00004d", "#009900", "#8c1aff", "#5c85d6", "#996600")

#update_geom_defaults("text", list(family = YSI_font, colour = YSI_palette[1]))
#scale_colour_discrete <- function(...) scale_colour_manual(..., values = YSI_palette)
#scale_fill_discrete <- function(...) scale_fill_manual(..., values = YSI_palette)
