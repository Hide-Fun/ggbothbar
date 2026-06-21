#' Academic theme for ggplot2
#'
#' A clean complete theme with bold labels based on theme_classic
#'
#' @param base_size Base font size
#' @param base_family Base font family ("Arial" by default)
#' @param base_face Base font face ("bold" by default)
#' @param text_color Text color ("black" by default)
#'
#' @return A ggplot2 theme
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_aca()
#' }
theme_aca <- function(
  base_size = 20,
  base_family = "Arial",
  base_face = "bold",
  text_color = "black"
) {
  # Start with theme_classic as base
  theme_classic(
    base_size = base_size,
    base_family = base_family
  ) %+replace%

    # Customize theme elements
    theme(
      # Remove legend
      legend.position = "none",

      # Format axis titles
      axis.title.x = element_text(
        size = base_size,
        face = base_face,
        color = text_color,
        margin = margin(2, 0, 0, 0, "mm")
      ),
      axis.title.y = element_text(
        angle = 90,
        size = base_size,
        face = base_face,
        color = text_color,
        margin = margin(0, 2, 0, 0, "mm")
      ),

      # Format axis text
      axis.text.x = element_text(
        color = text_color,
        size = base_size - 3,
        face = base_face
      ),
      axis.text.y = element_text(
        color = text_color,
        size = base_size - 3,
        face = base_face
      ),

      # Set plot margins
      plot.margin = margin(.5, .5, .5, .5, "mm"),

      # Set axis lines and ticks
      axis.line = element_line(linewidth = .7),
      axis.ticks = element_line(linewidth = .7)
    )
}

#' Isotope theme for ggplot2
#'
#' `theme_isotope()` is kept for backward compatibility. Use [theme_aca()]
#' for new code.
#'
#' @inheritParams theme_aca
#'
#' @return A ggplot2 theme
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_isotope()
#' }
theme_isotope <- function(
  base_size = 20,
  base_family = "Arial",
  base_face = "bold",
  text_color = "black"
) {
  theme_aca(
    base_size = base_size,
    base_family = base_family,
    base_face = base_face,
    text_color = text_color
  )
}
