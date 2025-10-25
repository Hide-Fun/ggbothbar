#' Draw a Reference Box on a ggplot
#'
#' This function adds a reference box to a ggplot, indicating the mean and error range for the specified x and y variables.

#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated because more flexibile function `geom_errorbox()` is added.
#'
#' @param p A ggplot object to which the reference box will be added
#' @param data A data frame containing the data
#' @param x A character string specifying the name of the x variable
#' @param y A character string specifying the name of the y variable
#' @param fill The fill color of the reference box. Defaults to "white"
#' @param colour The border color of the reference box. Defaults to "#028760"
#' @param fun.errorbar Error calculation method ("sd", "se", "ci" or a custom function). Defaults to "sd"
#' @param na.rm A logical value indicating whether NA values should be stripped before the computation proceeds. Defaults to FALSE
#' @param ... Other arguments passed on to \code{geom_rect()}
#' @return A ggplot object with the reference box added
#' @export
draw_reference_box <- function(
  p,
  data,
  x,
  y,
  fill = "white",
  colour = "#028760",
  fun.errorbar = "sd",
  na.rm = FALSE,
  ...
) {
  newdata <- data.frame(
    xmin = mean(data[[x]]) -
      calc_error(data[[x]], fun.errorbar = fun.errorbar, na.rm = na.rm),
    xmax = mean(data[[x]]) +
      calc_error(data[[x]], fun.errorbar = fun.errorbar, na.rm = na.rm),
    ymin = mean(data[[y]]) -
      calc_error(data[[y]], fun.errorbar = fun.errorbar, na.rm = na.rm),
    ymax = mean(data[[y]]) +
      calc_error(data[[y]], fun.errorbar = fun.errorbar, na.rm = na.rm)
  )
  p +
    geom_rect(
      data = newdata,
      mapping = aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      fill = fill,
      colour = colour,
      ...
    )
}
