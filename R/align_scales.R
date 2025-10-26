#' Align axis scales of multiple ggplot objects
#'
#' Ensures consistent axis limits and break intervals across multiple ggplot
#' objects. This is useful when comparing plots side-by-side or combining them
#' using packages such as `patchwork`, `gridExtra`, or `cowplot`.
#'
#' This function separates two concepts:
#' - The visible plotting range is controlled by `x_limits` / `y_limits`
#'   (applied via `coord_*()` as `xlim` / `ylim`).
#' - The scale padding is controlled by `expand`, which is passed to
#'   `scale_x_continuous(expand = ...)` and `scale_y_continuous(expand = ...)`.
#'
#' @param plots A list of ggplot objects (length >= 2).
#' @param axes Character vector specifying which axes to align. Must be any
#'   combination of `"x"` and/or `"y"`. For example:
#'   - `"x"`      to align only the x-axis
#'   - `"y"`      to align only the y-axis
#'   - `c("x","y")` to align both axes.
#'   Default: `c("x","y")`.
#' @param x_break_step Numeric. Step size for x-axis breaks (default: 3).
#' @param y_break_step Numeric. Step size for y-axis breaks (default: 3).
#' @param x_limits Optional numeric vector. x-axis visible limits, either
#'   `c(min, max)` or a single number. These are ultimately enforced via
#'   `coord_*()` and therefore define the displayed range.
#' @param y_limits Optional numeric vector. y-axis visible limits, either
#'   `c(min, max)` or a single number.
#' @param aspect_ratio Optional numeric. If specified, enforces a fixed aspect
#'   ratio by calling `coord_fixed()`. The ratio is scaled by the observed
#'   x/y spans.
#' @param clip Character. Passed to `coord_*()`. One of `"on"`, `"off"`,
#'   or `"inherit"` (default: `"off"`).
#' @param expand_breaks Logical. If `TRUE`, the max axis values are extended so
#'   that the span becomes an exact multiple of the break step. This ONLY
#'   affects the computed limits (xlim/ylim). It does NOT change `expand`.
#' @param expand Expansion specification passed to both `scale_x_continuous()`
#'   and `scale_y_continuous()` as their `expand` argument. For example,
#'   `expansion(mult = 0)` removes padding; `waiver()` uses ggplot2 defaults.
#'   Default: `waiver()`.
#'
#' @return A list of ggplot objects with aligned axis scales.
#'
#' @examples
#' library(ggplot2)
#' library(patchwork)
#'
#' # Example data
#' df4 <- subset(mtcars, cyl == 4)
#' df6 <- subset(mtcars, cyl == 6)
#'
#' p4 <- ggplot(df4, aes(wt, mpg)) +
#'   geom_point(color = "steelblue") +
#'   labs(title = "4 cylinders")
#'
#' p6 <- ggplot(df6, aes(wt, mpg)) +
#'   geom_point(color = "firebrick") +
#'   labs(title = "6 cylinders")
#'
#' ##
#' ## 1. Align both x and y (default behavior)
#' ##
#' aligned_both <- align_axis_scales(
#'   plots = list(p4, p6),
#'   axes = c("x", "y")  # align both axes
#' )
#'
#' # Both plots now share the same break positions and coord x/y ranges.
#' aligned_both[[1]] + aligned_both[[2]]
#'
#'
#' ##
#' ## 2. Control visible range with x_limits / y_limits
#' ##
#' # Here we *force* x to be from 2 to 5, regardless of each plot's data.
#' # y_limits is left NULL, so it comes from the data range.
#' #
#' # IMPORTANT:
#' #   The break sequence for x (seq(xmin, xmax, by = x_break_step))
#' #   is built from these merged limits.
#'
#' aligned_forced_x <- align_axis_scales(
#'   plots = list(p4, p6),
#'   axes = "x",
#'   x_limits = c(2, 5),
#'   x_break_step = 0.5
#' )
#'
#' # -> x axis will show ticks at 2.0, 2.5, 3.0, ..., 5.0,
#' #    and coord_cartesian(xlim = c(2, 5)) is applied to both.
#' aligned_forced_x[[1]] + aligned_forced_x[[2]]
#'
#'
#' ##
#' ## 3. expand_breaks = TRUE "rounds up" the max to a clean multiple
#' ##
#' # Suppose combined x-range of p4/p6 is about [1.5, 4.2].
#' # With x_break_step = 1, the raw span is about 2.7.
#' # 2.7 is *not* a multiple of 1, so we bump the upper bound
#' # up to 4.5 (or 5, depending on rounding),
#' # so that the final span is an exact multiple of 1.
#' #
#' # This gives "clean" tick marks like 1, 2, 3, 4, 5.
#'
#' aligned_rounded <- align_axis_scales(
#'   plots = list(p4, p6),
#'   axes = "x",
#'   x_break_step = 1,
#'   expand_breaks = TRUE
#' )
#'
#' aligned_rounded[[1]] + aligned_rounded[[2]]
#'
#'
#' ##
#' ## 4. expand controls the visual padding at plot edges
#' ##
#' # Case A: default padding (expand = waiver()).
#' #   ggplot2 usually adds ~5% space beyond the extreme values.
#'
#' aligned_default_pad <- align_axis_scales(
#'   plots = list(p4, p6),
#'   axes = c("x", "y"),
#'   x_break_step = 0.5,
#'   y_break_step = 5,
#'   expand = waiver()     # keep ggplot2 default padding
#' )
#'
#' # Case B: no padding at all (expand = expansion(mult = 0)).
#' #   The axes start *exactly* at the computed limits, i.e.,
#' #   the panel border touches the first/last break.
#'
#' aligned_no_pad <- align_axis_scales(
#'   plots = list(p4, p6),
#'   axes = c("x", "y"),
#'   x_break_step = 0.5,
#'   y_break_step = 5,
#'   expand = expansion(mult = 0)
#' )
#'
#' # Compare visually:
#' #   aligned_default_pad[[1]] vs aligned_no_pad[[1]]
#'
#'
#' ##
#' ## 5. Aspect ratio locking
#' ##
#' # aspect_ratio = 1 tries to make "1 unit of x equals 1 unit of y"
#' # after accounting for data ranges. This is useful for scatterplots
#' # where true geometric angles/distances matter.
#'
#' aligned_fixed_ratio <- align_axis_scales(
#'   plots = list(p4, p6),
#'   axes = c("x", "y"),
#'   x_break_step = 0.5,
#'   y_break_step = 5,
#'   aspect_ratio = 1
#' )
#'
#' aligned_fixed_ratio[[1]] + aligned_fixed_ratio[[2]]
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
  expand_breaks = FALSE,
  expand = waiver()
) {
  # validate axes: must be subset of c("x","y"); allow several.ok
  axes <- match.arg(axes, choices = c("x", "y"), several.ok = TRUE)

  # input validation ---------------------------------------------------------
  if (!is.list(plots) || length(plots) < 2) {
    stop("`plots` must be a list with at least two ggplot objects.")
  }
  if (!all(vapply(plots, inherits, logical(1), "gg"))) {
    stop("All elements of `plots` must be ggplot objects.")
  }

  adjust_x <- "x" %in% axes
  adjust_y <- "y" %in% axes

  # build plots to extract limits -------------------------------------------
  builds <- lapply(plots, ggplot_build)
  x_limits_all <- unlist(lapply(builds, get_lim, ax = "x"))
  y_limits_all <- unlist(lapply(builds, get_lim, ax = "y"))

  xmin <- floor(min(x_limits_all, na.rm = TRUE))
  xmax <- ceiling(max(x_limits_all, na.rm = TRUE))
  ymin <- floor(min(y_limits_all, na.rm = TRUE))
  ymax <- ceiling(max(y_limits_all, na.rm = TRUE))

  # merge with user-specified limits ----------------------------------------
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

  # optionally expand to nearest multiple of break step ---------------------
  if (expand_breaks && adjust_x) {
    x_span <- xmax - xmin
    x_rem <- x_span %% x_break_step
    if (!isTRUE(all.equal(x_rem, 0))) {
      xmax <- xmax + (x_break_step - x_rem)
    }
  }
  if (expand_breaks && adjust_y) {
    y_span <- ymax - ymin
    y_rem <- y_span %% y_break_step
    if (!isTRUE(all.equal(y_rem, 0))) {
      ymax <- ymax + (y_break_step - y_rem)
    }
  }

  # construct shared scales -------------------------------------------------
  shared_scales <- list()

  if (adjust_x) {
    shared_scales <- c(
      shared_scales,
      scale_x_continuous(
        breaks = seq(xmin, xmax, by = x_break_step),
        expand = expand
      )
    )
  }
  if (adjust_y) {
    shared_scales <- c(
      shared_scales,
      scale_y_continuous(
        breaks = seq(ymin, ymax, by = y_break_step),
        expand = expand
      )
    )
  }

  # coordinate system with visible limits -----------------------------------
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
    # compute ratio scaled by data spans
    x_range <- abs(xmax - xmin)
    y_range <- abs(ymax - ymin)
    ratio_val <- (x_range / y_range) * aspect_ratio
    do.call(coord_fixed, c(coord_args, list(ratio = ratio_val)))
  }

  shared_scales <- c(shared_scales, coord_obj)

  # apply shared components to every plot -----------------------------------
  lapply(plots, `+`, shared_scales)
}

#' @keywords internal
merge_lim <- function(auto_min, auto_max, user_lim) {
  # Merge automatic range with user-supplied limit spec.
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
get_lim <- function(b, ax) {
  b$layout$panel_params[[1]][[ax]]$limits
}

#' Adjust axis scales of a single ggplot object
#'
#' Adjusts axis scales of a single ggplot object with user-specified break
#' steps, limits, expansion, and aspect ratio. This function is useful when you
#' want to fine-tune the appearance of a single plot.
#'
#' As in `align_axis_scales()`, the visible plotting range is controlled by
#' `x_limits` / `y_limits` (applied via `coord_*()`), while scale padding is
#' controlled by `expand`.
#'
#' @param plot A ggplot object.
#' @param x_break_step Numeric. Step size for x-axis breaks (default: 3).
#' @param y_break_step Numeric. Step size for y-axis breaks (default: 3).
#' @param x_limits Optional numeric vector. x-axis visible limits, either
#'   `c(min, max)` or a single number.
#' @param y_limits Optional numeric vector. y-axis visible limits, either
#'   `c(min, max)` or a single number.
#' @param aspect_ratio Optional numeric. If specified, enforces a fixed aspect
#'   ratio via `coord_fixed()`.
#' @param clip Character. Passed to `coord_*()`. One of `"on"`, `"off"`,
#'   or `"inherit"` (default: `"off"`).
#' @param expand_breaks Logical. If `TRUE`, the max axis values are extended so
#'   that the span becomes an exact multiple of the break step. This ONLY
#'   affects the computed limits (xlim/ylim), not the scale padding.
#' @param expand Expansion specification passed to both `scale_x_continuous()`
#'   and `scale_y_continuous()` as their `expand` argument. For example,
#'   `expansion(mult = 0)` removes padding; `waiver()` uses ggplot2 defaults.
#'   Default: `waiver()`.
#'
#' @return A ggplot object with adjusted axis scales.
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   labs(title = "Miles per Gallon vs Weight")
#'
#' ##
#' ## 1. Basic usage with custom breaks
#' ##
#' p_basic <- adjust_axis_scales(
#'   plot = p,
#'   x_break_step = 0.5,
#'   y_break_step = 5
#' )
#'
#' # -> x breaks at (roughly) seq(xmin, xmax, by = 0.5),
#' #    y breaks at seq(ymin, ymax, by = 5),
#' #    where xmin/xmax/ymin/ymax come from the plot's data range.
#' p_basic
#'
#'
#' ##
#' ## 2. Force visible x/y ranges using x_limits / y_limits
#' ##
#' p_forced_limits <- adjust_axis_scales(
#'   plot = p,
#'   x_limits = c(2, 5),   # show only wt in [2, 5]
#'   y_limits = c(10, 35), # show only mpg in [10, 35]
#'   x_break_step = 0.5,
#'   y_break_step = 5
#' )
#'
#' # NOTE:
#' #   The tick breaks will now be seq(2, 5, by = 0.5) on x and
#' #   seq(10, 35, by = 5) on y, *not* based on the original data range.
#' p_forced_limits
#'
#'
#' ##
#' ## 3. Make tick spacing "pretty" with expand_breaks = TRUE
#' ##
#' # Suppose x_limits after merging are c(1.2, 4.7) and x_break_step = 1.
#' # Raw span = 3.5, which is not a multiple of 1.
#' # With expand_breaks = TRUE, we bump the max up so that
#' # (max - min) becomes a clean multiple of 1, e.g. to 4.2 -> 5.2, etc.
#' # This yields clean ticks at 1, 2, 3, 4, 5, ...
#'
#' p_pretty_ticks <- adjust_axis_scales(
#'   plot = p,
#'   x_break_step = 1,
#'   y_break_step = 5,
#'   expand_breaks = TRUE
#' )
#'
#' p_pretty_ticks
#'
#'
#' ##
#' ## 4. Control panel padding with expand
#' ##
#' # Case A: default expansion (waiver()) -> ~5% headroom/tailroom.
#' p_default_pad <- adjust_axis_scales(
#'   plot = p,
#'   x_break_step = 0.5,
#'   y_break_step = 5,
#'   expand = waiver()
#' )
#'
#' # Case B: no padding at all.
#' # The plotting panel starts exactly at the first break and ends exactly
#' # at the last break. Good for tightly packed comparisons.
#' p_no_pad <- adjust_axis_scales(
#'   plot = p,
#'   x_break_step = 0.5,
#'   y_break_step = 5,
#'   expand = expansion(mult = 0)
#' )
#'
#' # Case C: asymmetric padding using `add`.
#' # Here we add +2 on the high end of y, useful to leave "headroom"
#' # above the highest point for labels/annotations.
#' p_headroom <- adjust_axis_scales(
#'   plot = p,
#'   x_break_step = 0.5,
#'   y_break_step = 5,
#'   expand = expansion(add = c(0, 2))
#' )
#'
#' # Visually compare p_default_pad, p_no_pad, p_headroom
#'
#'
#' ##
#' ## 5. Lock aspect ratio
#' ##
#' # aspect_ratio = 1 means "treat one x-unit as the same visual length
#' # as one y-unit", after accounting for observed spans. This is common
#' # in scatterplots where slope should reflect actual numeric slope.
#'
#' p_fixed_ratio <- adjust_axis_scales(
#'   plot = p,
#'   x_break_step = 0.5,
#'   y_break_step = 5,
#'   aspect_ratio = 1
#' )
#'
#' p_fixed_ratio
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
  expand_breaks = FALSE,
  expand = waiver()
) {
  # Validate input -----------------------------------------------------------
  if (!inherits(plot, "gg")) {
    stop("`plot` must be a ggplot object.")
  }

  # Build plot to extract original limits -----------------------------------
  build <- ggplot_build(plot)
  x_limits_all <- get_lim(build, "x")
  y_limits_all <- get_lim(build, "y")

  xmin <- floor(min(x_limits_all, na.rm = TRUE))
  xmax <- ceiling(max(x_limits_all, na.rm = TRUE))
  ymin <- floor(min(y_limits_all, na.rm = TRUE))
  ymax <- ceiling(max(y_limits_all, na.rm = TRUE))

  # Merge with user-specified limits ----------------------------------------
  x_new <- merge_lim(xmin, xmax, x_limits)
  y_new <- merge_lim(ymin, ymax, y_limits)

  xmin <- x_new[1]
  xmax <- x_new[2]
  ymin <- y_new[1]
  ymax <- y_new[2]

  # Optionally expand to neat multiples of break steps ----------------------
  if (expand_breaks) {
    # x-axis
    x_span <- xmax - xmin
    x_rem <- x_span %% x_break_step
    if (!isTRUE(all.equal(x_rem, 0))) {
      xmax <- xmax + (x_break_step - x_rem)
    }

    # y-axis
    y_span <- ymax - ymin
    y_rem <- y_span %% y_break_step
    if (!isTRUE(all.equal(y_rem, 0))) {
      ymax <- ymax + (y_break_step - y_rem)
    }
  }

  # Scales with custom breaks and expansion ---------------------------------
  shared_scales <- list(
    scale_x_continuous(
      breaks = seq(xmin, xmax, by = x_break_step),
      expand = expand
    ),
    scale_y_continuous(
      breaks = seq(ymin, ymax, by = y_break_step),
      expand = expand
    )
  )

  # Coordinate system with visible limits -----------------------------------
  coord_args <- list(
    clip = clip,
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax)
  )

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
