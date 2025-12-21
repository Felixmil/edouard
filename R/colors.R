#' Edouard Pastel Color Palette
#'
#' A soft, pastel color palette for consistent visualization across the project.
#'
#' @return A named character vector of hex colors
#' @export
#'
#' @examples
#' edouard_colors()
edouard_colors <- function() {
  c(
    "#FF9999",  # Coral pink
    "#99E380",  # Mint green
    "#80CCE6",  # Sky blue
    "#D9A6FF",  # Lavender
    "#FFD480",  # Peach yellow
    "#FFADD2",  # Rose
    "#A6E6C4",  # Sage
    "#B3CCFF"   # Periwinkle
  )
}

#' Get Edouard Color Palette for ggplot2
#'
#' Returns a ggplot2 scale function using the Edouard pastel color palette.
#'
#' @param discrete Logical. If TRUE, returns discrete scale. If FALSE, returns continuous scale.
#' @param ... Additional arguments passed to ggplot2 scale functions
#'
#' @return A ggplot2 scale function
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_edouard()
scale_color_edouard <- function(discrete = TRUE, ...) {
  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = "colour",
      scale_name = "edouard",
      palette = function(n) edouard_colors()[1:n],
      ...
    )
  } else {
    ggplot2::scale_color_gradientn(
      colours = edouard_colors(),
      ...
    )
  }
}

#' @rdname scale_color_edouard
#' @export
scale_colour_edouard <- scale_color_edouard

#' Get Edouard Fill Palette for ggplot2
#'
#' Returns a ggplot2 fill scale function using the Edouard pastel color palette.
#'
#' @param discrete Logical. If TRUE, returns discrete scale. If FALSE, returns continuous scale.
#' @param ... Additional arguments passed to ggplot2 scale functions
#'
#' @return A ggplot2 scale function
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_edouard()
scale_fill_edouard <- function(discrete = TRUE, ...) {
  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = "fill",
      scale_name = "edouard",
      palette = function(n) edouard_colors()[1:n],
      ...
    )
  } else {
    ggplot2::scale_fill_gradientn(
      colours = edouard_colors(),
      ...
    )
  }
}
