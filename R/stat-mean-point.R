#' Statistical Transformation for Mean Points
#'
#' This function computes the mean x and y coordinates for a group and displays them as points on a ggplot.
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()} or \code{aes_()}. If specified and \code{inherit.aes = TRUE} (the default), it is combined with the default mapping at the top level of the plot. You must supply \code{mapping} if there is no plot mapping
#' @param data The data to be displayed in this layer. There are three options: If \code{NULL}, the default, the data is inherited from the plot data as specified in the call to \code{ggplot()}. A \code{data.frame}, or other object, will override the plot data. All objects will be fortified to produce a data frame. See \code{fortify()} for which variables will be created. A \code{function} will be called with a single argument, the plot data. The return value must be a \code{data.frame}, and will be used as the layer data
#' @param geom The geometric object to use to display the data. Defaults to "point"
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function
#' @param na.rm If \code{FALSE}, the default, removes missing values with a warning. If \code{TRUE}, silently removes missing values
#' @param show.legend Logical. Should this layer be included in the legends? \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE} never includes, and \code{TRUE} always includes
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g., \code{borders()}
#' @param ... Other arguments passed on to \code{layer()}. These are often aesthetics, used to set an aesthetic to a fixed value, like \code{colour = "red"} or \code{size = 3}
#' @return A ggplot2 layer that adds mean points to a plot
#' @examples
#' library(ggplot2)
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#' ggplot(data, aes(x, y)) +
#'   geom_point() +
#'   stat_mean_point()
#' @export
stat_mean_point <- function(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    stat = StatMeanPoint,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @import ggplot2
StatMeanPoint <- ggproto(
  "StatMeanPoint",
  Stat,
  compute_group = function(data, scales, na.rm = FALSE) {
    data.frame(
      x = mean(data$x, na.rm = na.rm),
      y = mean(data$y, na.rm = na.rm)
    )
  },
  required_aes = c("x", "y")
)
