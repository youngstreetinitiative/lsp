#' Image fixture for RMarkdown
#'
#' @name image_fixer
#' @param path Character, file path to img as string.
#' @param fig_width_cm Numeric, number in cm of width desired.
#'
#' @rdname image_fixer
#' @export

image_fixer <- function(path, fig_width_cm = NULL) {


  if (is.null(fig_width_cm)) {
    knitr::include_graphics(path)
  } else {

    # check img pixels width
    img <- readbitmap::read.bitmap(path)
    width_px <- dim(img) %>%
      purrr::set_names(c("height", "width", "depth")) %>%
      .["width"]

    # calculate the required dpi val for expected cm width and image pixel width count
    custom_dpi  <- 1/((fig_width_cm / 2.54) / width_px)

    # generate image with found dpi
    knitr::include_graphics(path, dpi = custom_dpi)

  }

}

#' @rdname image_fixer
#' @export

ImageFixer <- image_fixer
