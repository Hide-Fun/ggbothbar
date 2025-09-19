#' Create an Error Bar Grob
#'
#' This function creates a graphical object (grob) representing error bars for use in grid graphics.
#'
#' @param x A numeric or unit object specifying the x-coordinates of the error bars. Defaults to \code{unit(0, "npc")}
#' @param y A numeric or unit object specifying the y-coordinates of the error bars. Defaults to \code{unit(0, "npc")}
#' @param fun.errorbar A character string indicating the method to calculate the error bar. It can be either "sd" for standard deviation or "se" for standard error. Defaults to "sd"
#' @param na.rm A logical value indicating whether NA values should be stripped before the computation proceeds. Defaults to FALSE
#' @param errorbar_tip_size A unit object specifying the size of the error bar tips. Defaults to \code{unit(0.1, "npc")}
#' @param default.units A character string indicating the default units to use if \code{x}, \code{y}, or \code{errorbar_tip_size} are not unit objects. Defaults to "npc"
#' @param name A character string specifying a name for the grob
#' @param gp An object of class \code{gpar}, typically the output from a call to the \code{gpar()} function, used to specify graphical parameters
#' @param vp A \code{viewport} object (or NULL)
#' @return A gTree object of class "errorbarb"
#' @examples
#' library(grid)
#'
#' df <- data.frame(
#'   x = c(.4, .1),
#'   y = c(.1, .5)
#' )
#' errorbar_grob <- errorbarbGrob(
#'   x = df$x, y = df$y,
#'   fun.errorbar = "sd",
#'   errorbar_tip_size = unit(5, "cm")
#' )
#' grid.draw(errorbar_grob)
#' @export
errorbarbGrob <- function(x = grid::unit(0, "npc"),
                          y = grid::unit(0, "npc"),
                          fun.errorbar = "sd",
                          na.rm = FALSE,
                          errorbar_tip_size = grid::unit(0.1, "npc"),
                          default.units = "npc",
                          name = NULL,
                          gp = grid::gpar(),
                          vp = NULL) {
  # Use the default unit if the user does not specify one
  if (!grid::is.unit(x)) x <- grid::unit(x, default.units)
  if (!grid::is.unit(y)) y <- grid::unit(y, default.units)
  if (!grid::is.unit(errorbar_tip_size)) errorbar_tip_size <- grid::unit(errorbar_tip_size, default.units)

  # Return a gTree of class "errorbarb"
  grid::gTree(
    x = x,
    y = y,
    fun.errorbar = fun.errorbar,
    na.rm = na.rm,
    errorbar_tip_size = errorbar_tip_size,
    name = name,
    gp = gp,
    vp = vp,
    cl = "errorbarb"
  )
}

#' Create Content for Error Bar Grob
#'
#' This function defines the content of the error bar grob, calculating the positions and dimensions of the error bars.
#'
#' @param grob An object of class "errorbarb"
#' @return A gTree object with the children set to the calculated error bars
#' @importFrom grid makeContent
#' @export makeContent.errorbarb
#' @export
makeContent.errorbarb <- function(grob) {
  # Convert position and diameter values absolute units
  x <- grid::convertX(grob$x, "mm", valueOnly = TRUE)
  y <- grid::convertY(grob$y, "mm", valueOnly = TRUE)
  errorbar_tip_size <- grid::convertUnit(grob$errorbar_tip_size, "cm", valueOnly = TRUE)

  fun.errorbar <- grob$fun.errorbar
  na.rm <- grob$na.rm

  # summarise mean & sd or se
  # calculate coordination
  if (fun.errorbar == "sd") {
    errorbarb <- create_errorbarb(
      x = mean(x, na.rm = na.rm),
      y = mean(y, na.rm = na.rm),
      height = stats::sd(y, na.rm = na.rm),
      width = stats::sd(x, na.rm = na.rm),
      errorbar_tip_size = errorbar_tip_size
    )
  } else if (fun.errorbar == "se") {
    errorbarb <- create_errorbarb(
      x = mean(x, na.rm = na.rm),
      y = mean(y, na.rm = na.rm),
      height = se(y, na.rm = na.rm),
      width = se(x, na.rm = na.rm),
      errorbar_tip_size = errorbar_tip_size
    )
  }

  # Construct the grob
  errorbarb_line <- grid::segmentsGrob(
    x0 = errorbarb$x,
    y0 = errorbarb$y,
    x1 = errorbarb$xend,
    y1 = errorbarb$yend,
    default.units = "mm",
    gp = grob$gp
  )

  grid::setChildren(grob, grid::gList(errorbarb_line))
}

#' Create Coordinates for Error Bars with Custom Tips
#'
#' This function generates the coordinates required to draw custom error bars with specified height, width, and tip size.
#'
#' @param x Numeric value representing the x-coordinate of the center of the error bar
#' @param y Numeric value representing the y-coordinate of the center of the error bar
#' @param height Numeric value representing the height of the error bar
#' @param width Numeric value representing the width of the error bar
#' @param errorbar_tip_size Numeric value representing the size of the tips of the error bar
#' @return A data frame containing the coordinates of the error bar segments
#' @examples
#' errorbar_coords <- create_errorbarb(0.5, 0.5, 0.1, 0.05, 0.02)
#' print(errorbar_coords)
#' @export
create_errorbarb <- function(x, y, height, width, errorbar_tip_size) {
  # validate the input arguments
  if (height < 0) {
    rlang::abort("`height` must be non-negative.")
  }
  if (width < 0) {
    rlang::abort("`width` must be non-negative.")
  }
  if (errorbar_tip_size < 0) {
    rlang::abort("`errorbar_tip_size` must be non-negative.")
  }
  # calculate coordination of errorbarb
  data.frame(
    x = c(
      x - errorbar_tip_size / 2,
      x - errorbar_tip_size / 2,
      x - width,
      x,
      x - width,
      x + width,
      x,
      x
    ),
    xend = c(
      x + errorbar_tip_size / 2,
      x + errorbar_tip_size / 2,
      x,
      x + width,
      x - width,
      x + width,
      x,
      x
    ),
    y = c(
      y + height,
      y - height,
      y,
      y,
      y - errorbar_tip_size / 2,
      y - errorbar_tip_size / 2,
      y,
      y
    ),
    yend = c(
      y + height,
      y - height,
      y,
      y,
      y + errorbar_tip_size / 2,
      y + errorbar_tip_size / 2,
      y + height,
      y - height
    )
  )
}
