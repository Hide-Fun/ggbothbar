#' Align axis scales of multiple ggplot objects
#'
#' Ensures consistent axis limits and break intervals across multiple ggplot objects.
#' This function is useful when comparing plots side-by-side or combining them using
#' packages such as `patchwork`, `gridExtra`, or `cowplot`.
#'
#' @param plots A list of ggplot objects (length >= 2).
#' @param axes Character. Which axes to align. One of `"x"`, `"y"`, or `"both"` (default).
#' @param x_break_step Numeric. Step size for x-axis breaks (default: 3).
#' @param y_break_step Numeric. Step size for y-axis breaks (default: 3).
#' @param x_limits Optional numeric vector. x-axis limits, either `c(min, max)` or a single number.
#' @param y_limits Optional numeric vector. y-axis limits, either `c(min, max)` or a single number.
#' @param aspect_ratio Optional numeric. If specified, enforces a fixed aspect ratio by calling `coord_fixed()`.
#' @param clip Character. Passed to `coord_*()`. One of `"on"`, `"off"`, or `"inherit"` (default: `"off"`).
#' @param expand_breaks Logical. If `TRUE`, the maximum axis values are expanded so the range is
#'   an exact multiple of the break step.
#'
#' @return A list of ggplot objects with aligned axis scales.
#'
#' @examples
#' library(ggplot2)
#' library(patchwork)
#'
#' # Prepare example datasets
#' df4 <- subset(mtcars, cyl == 4)
#' df6 <- subset(mtcars, cyl == 6)
#'
#' # Create ggplot objects
#' p4 <- ggplot(df4, aes(wt, mpg)) +
#'   geom_point(color = "steelblue") +
#'   labs(title = "4 cylinders")
#'
#' p6 <- ggplot(df6, aes(wt, mpg)) +
#'   geom_point(color = "firebrick") +
#'   labs(title = "6 cylinders")
#'
#' # Align axis scales (adjust both axes, fix aspect ratio)
#' aligned <- align_axis_scales(
#'   plots         = list(p4, p6),
#'   axes          = "both",
#'   x_break_step  = 0.5,
#'   y_break_step  = 5,
#'   aspect_ratio  = 1,
#'   expand_breaks = TRUE
#' )
#'
#' # Plot aligned results
#' aligned[[1]] + aligned[[2]]
#'
#' # Align only x-axis
#' aligned_x <- align_axis_scales(
#'   plots        = list(p4, p6),
#'   axes         = "x",
#'   x_break_step = 0.5
#' )
#' aligned_x[[1]] + aligned_x[[2]]
#'
#' @export
align_axis_scales <- function(
  plots,
  axes = c("x", "y"),
  x_break_step = 3,
  y_break_step = 3,
  x_limits = NULL,
  y_limits = NULL,
  aspect_ratio = NULL,
  clip = "off",
  expand_breaks = FALSE
) {
  axes <- match.arg(axes, choices = c("x", "y", "both"))

  if (!is.list(plots) || length(plots) < 2) {
    stop("`plots` must be a list with at least two ggplot objects.")
  }
  if (!all(vapply(plots, inherits, logical(1), "gg"))) {
    stop("All elements of `plots` must be ggplot objects.")
  }

  adjust_x <- axes %in% c("x", "both")
  adjust_y <- axes %in% c("y", "both")

  builds <- lapply(plots, ggplot_build)
  x_limits_all <- unlist(lapply(builds, get_lim, ax = "x"))
  y_limits_all <- unlist(lapply(builds, get_lim, ax = "y"))

  xmin <- floor(min(x_limits_all, na.rm = TRUE))
  xmax <- ceiling(max(x_limits_all, na.rm = TRUE))
  ymin <- floor(min(y_limits_all, na.rm = TRUE))
  ymax <- ceiling(max(y_limits_all, na.rm = TRUE))

  if (adjust_x) {
    x_new <- merge_lim(xmin, xmax, x_limits)
    xmin <- x_new[1]
    xmax <- x_new[2]
  }
  if (adjust_y) {
    y_new <- merge_lim(ymin, ymax, y_limits)
    ymin <- y_new[1]
    ymax <- y_new[2]
  }

  if (expand_breaks && adjust_x) {
    span <- xmax - xmin
    rem <- span %% x_break_step
    if (!isTRUE(all.equal(rem, 0))) {
      xmax <- xmax + (x_break_step - rem)
    }
  }
  if (expand_breaks && adjust_y) {
    span <- ymax - ymin
    rem <- span %% y_break_step
    if (!isTRUE(all.equal(rem, 0))) {
      ymax <- ymax + (y_break_step - rem)
    }
  }

  shared_scales <- list()
  expand_opt <- if (expand_breaks) expansion(mult = 0) else waiver()

  if (adjust_x) {
    shared_scales <- c(
      shared_scales,
      scale_x_continuous(
        breaks = seq(xmin, xmax, by = x_break_step),
        expand = expand_opt
      )
    )
  }
  if (adjust_y) {
    shared_scales <- c(
      shared_scales,
      scale_y_continuous(
        breaks = seq(ymin, ymax, by = y_break_step),
        expand = expand_opt
      )
    )
  }

  coord_args <- list(clip = clip)
  if (adjust_x) {
    coord_args$xlim <- c(xmin, xmax)
  }
  if (adjust_y) {
    coord_args$ylim <- c(ymin, ymax)
  }

  coord_obj <- if (is.null(aspect_ratio)) {
    do.call(coord_cartesian, coord_args)
  } else {
    x_range <- abs(xmax - xmin)
    y_range <- abs(ymax - ymin)
    ratio_val <- (x_range / y_range) * aspect_ratio
    do.call(coord_fixed, c(coord_args, list(ratio = ratio_val)))
  }

  shared_scales <- c(shared_scales, coord_obj)

  lapply(plots, `+`, shared_scales)
}

#' @keywords internal
merge_lim <- function(auto_min, auto_max, user_lim) {
  if (is.null(user_lim)) {
    return(c(auto_min, auto_max))
  }
  if (!is.numeric(user_lim)) {
    warning("Limits must be numeric. Using automatic limits.")
    return(c(auto_min, auto_max))
  }
  if (length(user_lim) == 2) {
    return(user_lim)
  }
  if (length(user_lim) == 1) {
    if (user_lim <= auto_min) {
      return(c(user_lim, auto_max))
    }
    if (user_lim >= auto_max) {
      return(c(auto_min, user_lim))
    }
    return(c(user_lim, auto_max))
  }
  warning("Use a scalar or c(min, max). Using automatic limits.")
  c(auto_min, auto_max)
}

#' @keywords internal
get_lim <- function(b, ax) b$layout$panel_params[[1]][[ax]]$limits

#' Adjust axis scales of a single ggplot object
#'
#' Adjusts axis scales of a single ggplot object with user-specified break steps,
#' limits, and aspect ratio. This function is useful when you want to fine-tune
#' the appearance of a single plot.
#'
#' @param plot A ggplot object.
#' @param x_break_step Numeric. Step size for x-axis breaks (default: 3).
#' @param y_break_step Numeric. Step size for y-axis breaks (default: 3).
#' @param x_limits Optional numeric vector. x-axis limits, either `c(min, max)` or a single number.
#' @param y_limits Optional numeric vector. y-axis limits, either `c(min, max)` or a single number.
#' @param aspect_ratio Optional numeric. If specified, enforces a fixed aspect ratio by calling `coord_fixed()`.
#' @param clip Character. Passed to `coord_*()`. One of `"on"`, `"off"`, or `"inherit"` (default: `"off"`).
#' @param expand_breaks Logical. If `TRUE`, the maximum axis values are expanded so the range is
#'   an exact multiple of the break step.
#'
#' @return A ggplot object with adjusted axis scales.
#'
#' @examples
#' library(ggplot2)
#'
#' # Create a sample plot
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   labs(title = "Miles per Gallon vs Weight")
#'
#' # Adjust axis scales with custom break steps and limits
#' p_adjusted <- adjust_axis_scales(
#'   plot          = p,
#'   x_break_step  = 0.5,
#'   y_break_step  = 5,
#'   x_limits      = c(1, 6),
#'   y_limits      = c(10, 35),
#'   aspect_ratio  = 1,
#'   expand_breaks = TRUE
#' )
#'
#' # Display the adjusted plot
#' p_adjusted
#'
#' @export
adjust_axis_scales <- function(
  plot,
  x_break_step = 3,
  y_break_step = 3,
  x_limits = NULL,
  y_limits = NULL,
  aspect_ratio = NULL,
  clip = "off",
  expand_breaks = FALSE
) {
  if (!inherits(plot, "gg")) {
    stop("`plot` must be a ggplot object.")
  }

  build <- ggplot_build(plot)
  x_limits_all <- get_lim(build, "x")
  y_limits_all <- get_lim(build, "y")

  xmin <- floor(min(x_limits_all, na.rm = TRUE))
  xmax <- ceiling(max(x_limits_all, na.rm = TRUE))
  ymin <- floor(min(y_limits_all, na.rm = TRUE))
  ymax <- ceiling(max(y_limits_all, na.rm = TRUE))

  x_new <- merge_lim(xmin, xmax, x_limits)
  y_new <- merge_lim(ymin, ymax, y_limits)

  xmin <- x_new[1]
  xmax <- x_new[2]
  ymin <- y_new[1]
  ymax <- y_new[2]

  if (expand_breaks) {
    # Adjust x-axis
    span <- xmax - xmin
    rem <- span %% x_break_step
    if (!isTRUE(all.equal(rem, 0))) {
      xmax <- xmax + (x_break_step - rem)
    }

    # Adjust y-axis
    span <- ymax - ymin
    rem <- span %% y_break_step
    if (!isTRUE(all.equal(rem, 0))) {
      ymax <- ymax + (y_break_step - rem)
    }
  }

  shared_scales <- list()
  expand_opt <- if (expand_breaks) expansion(mult = 0) else waiver()

  shared_scales <- c(
    shared_scales,
    scale_x_continuous(
      breaks = seq(xmin, xmax, by = x_break_step),
      expand = expand_opt
    ),
    scale_y_continuous(
      breaks = seq(ymin, ymax, by = y_break_step),
      expand = expand_opt
    )
  )

  coord_args <- list(clip = clip, xlim = c(xmin, xmax), ylim = c(ymin, ymax))

  coord_obj <- if (is.null(aspect_ratio)) {
    do.call(coord_cartesian, coord_args)
  } else {
    x_range <- abs(xmax - xmin)
    y_range <- abs(ymax - ymin)
    ratio_val <- (x_range / y_range) * aspect_ratio
    do.call(coord_fixed, c(coord_args, list(ratio = ratio_val)))
  }

  shared_scales <- c(shared_scales, coord_obj)

  plot + shared_scales
}
