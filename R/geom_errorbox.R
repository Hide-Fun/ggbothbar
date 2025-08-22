#' ggproto object for error box statistics
#'
#' @format ggproto class
#' @importFrom ggplot2 ggproto
#' @keywords internal
StatErrorbox <- ggplot2::ggproto("StatErrorbox", ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales, fun.errorbar = "sd", na.rm = FALSE) {
    x <- data$x
    y <- data$y

    data.frame(
      x = mean(x, na.rm = na.rm),
      y = mean(y, na.rm = na.rm),
      xmin = mean(x, na.rm = na.rm) - calc_error(x, fun.errorbar = fun.errorbar, na.rm = na.rm),
      xmax = mean(x, na.rm = na.rm) + calc_error(x, fun.errorbar = fun.errorbar, na.rm = na.rm),
      ymin = mean(y, na.rm = na.rm) - calc_error(y, fun.errorbar = fun.errorbar, na.rm = na.rm),
      ymax = mean(y, na.rm = na.rm) + calc_error(y, fun.errorbar = fun.errorbar, na.rm = na.rm)
    )
  }
)

#' Add error boxes to a plot
#'
#' This function draws a rectangular error box centered at the mean of x and y coordinates,
#' with dimensions determined by a specified error calculation method (standard deviation,
#' standard error, 95% confidence interval, etc.).
#'
#' @param mapping Set of aesthetic mappings, usually created with `aes()`
#' @param data The data to be displayed. If NULL, the default, the data is inherited from the plot data
#' @param stat The statistical transformation to use on the data
#' @param position Position adjustment
#' @param ... Other arguments passed to `GeomRect`
#' @param fun.errorbar Error calculation method ("sd", "se", "ci" or a custom function)
#' @param na.rm If FALSE, the default, missing values are removed with a warning
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics
#'
#' @details
#' The error box is drawn as a rectangle spanning from xmin to xmax and ymin to ymax,
#' where these boundaries are calculated based on the specified error method.
#' The box is centered at the mean values of x and y coordinates.
#'
#' Error calculation methods:
#' \itemize{
#'   \item "sd": Standard deviation
#'   \item "se": Standard error (standard deviation divided by square root of the number of observations)
#'   \item "ci": 95\% confidence interval (based on the t-distribution)
#'   \item custom function: You can provide your own error calculation function
#' }
#'
#' @return A ggplot2 layer
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + geom_point() +
#'   geom_errorbox(fill = "white", color = "blue", alpha = 0.3)
#'
#' # Using standard deviation (default)
#' p + geom_point() +
#'   geom_errorbox(fill = "white", color = "red", alpha = 0.2)
#'
#' # Using standard error
#' p + geom_point() +
#'   geom_errorbox(fill = "white", color = "green", alpha = 0.2, fun.errorbar = "se")
#'
#' # Using 95% confidence interval
#' p + geom_point() +
#'   geom_errorbox(fill = "white", color = "purple", alpha = 0.2, fun.errorbar = "ci")
#'
#' # Error boxes by group
#' p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl)))
#' p + geom_point() +
#'   geom_errorbox(aes(fill = factor(cyl)), alpha = 0.2)
#' }
#'
#' @export
#' @importFrom ggplot2 layer GeomRect
geom_errorbox <- function(mapping = NULL, data = NULL, stat = "errorbox",
                          position = "identity", ...,
                          fun.errorbar = "sd", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatErrorbox,
    geom = ggplot2::GeomRect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun.errorbar = fun.errorbar,
      na.rm = na.rm,
      ...
    )
  )
}
