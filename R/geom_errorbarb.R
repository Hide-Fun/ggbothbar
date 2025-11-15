#' Geom for Error Bars (Both x & y axis) with Custom Tips
#'
#' This function creates a custom ggplot2 geom for drawing error bars with specified error calculation methods and tip sizes.
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()} or \code{aes_()}. If specified and \code{inherit.aes = TRUE} (the default), it is combined with the default mapping at the top level of the plot. You must supply \code{mapping} if there is no plot mapping
#' @param data The data to be displayed in this layer. There are three options: If \code{NULL}, the default, the data is inherited from the plot data as specified in the call to \code{ggplot()}. A \code{data.frame}, or other object, will override the plot data. All objects will be fortified to produce a data frame. See \code{fortify()} for which variables will be created. A \code{function} will be called with a single argument, the plot data. The return value must be a \code{data.frame}, and will be used as the layer data
#' @param stat The statistical transformation to use on the data for this layer, as a string
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function
#' @param ... Other arguments passed on to \code{layer()}. These are often aesthetics, used to set an aesthetic to a fixed value, like \code{colour = "red"} or \code{size = 3}. They may also be parameters to the paired geom/stat
#' @param fun.errorbar A character string indicating the method to calculate the error bar. It can be either "sd" for standard deviation or "se" for standard error. Defaults to "sd"
#' @param na.rm If \code{FALSE}, the default, removes missing values with a warning. If \code{TRUE} silently removes missing values
#' @param errorbar_tip_size A numeric value specifying the size of the error bar tips in cm. Defaults to 2
#' @param lineend Line end style (round, butt, square). Default is "butt"
#' @param linewidth Width of lines. Default is 0.5
#' @param show.legend Logical. Should this layer be included in the legends? \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE} never includes, and \code{TRUE} always includes
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g., \code{borders()}
#' @return A ggplot2 layer
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_errorbarb()
#' @export
geom_errorbarb <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  fun.errorbar = "sd",
  na.rm = FALSE,
  errorbar_tip_size = 2,
  lineend = "butt",
  linewidth = .5,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = Geomerrorbarb,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun.errorbar = fun.errorbar,
      na.rm = na.rm,
      errorbar_tip_size = errorbar_tip_size,
      linewidth = linewidth,
      ...
    )
  )
}

#' Geomerrorbarb ggproto object
#'
#' Internal ggproto backing `geom_errorbarb()`. Most users should call
#' `geom_errorbarb()` directly rather than constructing this object manually.
#'
#' @format NULL
#' @usage NULL
#' @family ggplot2 geoms
#' @importFrom grid nullGrob unit
#' @export
Geomerrorbarb <- ggproto(
  "Geomerrorbarb",
  Geom,

  # Transform the data inside the draw_panel() method
  draw_group = function(
    data,
    panel_params,
    coord,
    fun.errorbar = "sd",
    na.rm = FALSE,
    errorbar_tip_size = 2,
    lineend = "butt"
  ) {
    if (is.null(data) || nrow(data) == 0) {
      return(nullGrob())
    }
    # Supply the coordinate system for the plot
    if (!coord$is_linear()) {
      rlang::warn(
        "spring geom only works correctly on linear coordinate systems"
      )
    }
    coord <- coord$transform(data, panel_params)

    # Construct the grob
    errorbarbGrob(
      coord$x,
      coord$y,
      default.units = "native",
      fun.errorbar = fun.errorbar,
      na.rm = na.rm,
      errorbar_tip_size = unit(errorbar_tip_size, "cm"),
      gp = grid::gpar(
        col = alpha(coord$colour, coord$alpha),
        lwd = coord$linewidth * ggplot2::.pt,
        lty = coord$linetype,
        lineend = lineend
      )
    )
  },
  draw_key = draw_key_path,

  # Specify the default and required aesthetics
  required_aes = c("x", "y"),
  default_aes = aes(
    colour = "black",
    linewidth = .5,
    linetype = 1L,
    alpha = NA,
    fun.errorbar = "sd",
    errorbar_tip_size = 2
  )
)
