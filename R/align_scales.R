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
#' Only continuous axes can be aligned. If an axis requested via `axes`
#' is discrete (factor / character) in any of the plots, the function stops
#' with an informative error rather than partially modifying the scales.
#' In that case, convert the scale to numeric first or remove the axis
#' from `axes`.
#'
#' `align_axis_scales()` returns a list of ggplot objects in the same order
#' they were supplied. Each plot receives a shared `scale_*_continuous()`
#' plus either `coord_cartesian()` or `coord_fixed()` (if `aspect_ratio`
#' is set) so that the visible range, break spacing, and expansion are
#' consistent across the collection.
#' When `expand_breaks = TRUE`, the upper bound of each continuous span is
#' increased just enough so that `(max - min)` becomes an exact multiple of the
#' relevant break step, yielding "clean" tick positions while keeping padding
#' control in `expand`.
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
#' @return A list of ggplot objects with aligned axis scales (same length and
#'   order as the input `plots` list).
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
  # validate axes -----------------------------------------------------------
  axes <- match.arg(axes, choices = c("x", "y"), several.ok = TRUE)

  # basic checks ------------------------------------------------------------
  if (!is.list(plots) || length(plots) < 2) {
    stop("`plots` must be a list with at least two ggplot objects.")
  }
  if (!all(vapply(plots, inherits, logical(1), "gg"))) {
    stop("All elements of `plots` must be ggplot objects.")
  }

  adjust_x <- "x" %in% axes
  adjust_y <- "y" %in% axes

  # Build plots once --------------------------------------------------------
  builds <- lapply(plots, ggplot_build)

  # We'll compute aligned ranges only for axes we adjust.
  # For aspect_ratio we may still need spans of the non-adjusted axis,
  # so we will keep a reference to the first plot's limits.

  # x-axis handling ---------------------------------------------------------
  if (adjust_x) {
    x_limits_all <- unlist(lapply(builds, get_lim, ax = "x"), use.names = FALSE)

    if (!is.numeric(x_limits_all)) {
      stop(
        "`axes` includes 'x', but at least one plot has a discrete x scale. ",
        "Currently align_axis_scales() only supports continuous x when aligning x."
      )
    }

    xmin <- floor(min(x_limits_all, na.rm = TRUE))
    xmax <- ceiling(max(x_limits_all, na.rm = TRUE))

    x_new <- merge_lim(xmin, xmax, x_limits)
    xmin <- x_new[1]
    xmax <- x_new[2]

    if (expand_breaks) {
      x_span <- xmax - xmin
      x_rem <- x_span %% x_break_step
      if (!isTRUE(all.equal(x_rem, 0))) {
        xmax <- xmax + (x_break_step - x_rem)
      }
    }
  } else {
    # not aligning x
    xmin <- xmax <- NULL
    # but we still want reference limits for aspect_ratio
    x_limits_first <- get_lim(builds[[1]], "x")
  }

  # y-axis handling ---------------------------------------------------------
  if (adjust_y) {
    y_limits_all <- unlist(lapply(builds, get_lim, ax = "y"), use.names = FALSE)

    if (!is.numeric(y_limits_all)) {
      stop(
        "`axes` includes 'y', but at least one plot has a discrete y scale. ",
        "Currently align_axis_scales() only supports continuous y when aligning y."
      )
    }

    ymin <- floor(min(y_limits_all, na.rm = TRUE))
    ymax <- ceiling(max(y_limits_all, na.rm = TRUE))

    y_new <- merge_lim(ymin, ymax, y_limits)
    ymin <- y_new[1]
    ymax <- y_new[2]

    if (expand_breaks) {
      y_span <- ymax - ymin
      y_rem <- y_span %% y_break_step
      if (!isTRUE(all.equal(y_rem, 0))) {
        ymax <- ymax + (y_break_step - y_rem)
      }
    }
  } else {
    # not aligning y
    ymin <- ymax <- NULL
    # reference limits for aspect_ratio
    y_limits_first <- get_lim(builds[[1]], "y")
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

  # aspect_ratio handling ---------------------------------------------------
  coord_obj <- if (is.null(aspect_ratio)) {
    do.call(coord_cartesian, coord_args)
  } else {
    # We need spans for x and y. We may NOT have aligned an axis,
    # so fall back to the first plot's build for that axis.
    #
    # x info:
    if (adjust_x) {
      x_span_ratio <- axis_span_for_ratio(
        is_num = TRUE, # we stopped() earlier if not numeric
        final_min = xmin,
        final_max = xmax,
        lims_all = NULL
      )
    } else {
      x_all_first <- x_limits_first
      x_is_num_first <- is.numeric(x_all_first)

      if (x_is_num_first) {
        # use the raw numeric span from first plot
        x_rng <- range(x_all_first, na.rm = TRUE)
        x_span_tmp <- abs(diff(x_rng))
        if (x_span_tmp == 0) {
          x_span_tmp <- 1
        }
        x_span_ratio <- x_span_tmp
      } else {
        # treat discrete levels as 1,2,3,...
        x_span_ratio <- axis_span_for_ratio(
          is_num = FALSE,
          final_min = NA_real_,
          final_max = NA_real_,
          lims_all = x_all_first
        )
      }
    }

    # y info:
    if (adjust_y) {
      y_span_ratio <- axis_span_for_ratio(
        is_num = TRUE, # we stopped() earlier if not numeric
        final_min = ymin,
        final_max = ymax,
        lims_all = NULL
      )
    } else {
      y_all_first <- y_limits_first
      y_is_num_first <- is.numeric(y_all_first)

      if (y_is_num_first) {
        y_rng <- range(y_all_first, na.rm = TRUE)
        y_span_tmp <- abs(diff(y_rng))
        if (y_span_tmp == 0) {
          y_span_tmp <- 1
        }
        y_span_ratio <- y_span_tmp
      } else {
        y_span_ratio <- axis_span_for_ratio(
          is_num = FALSE,
          final_min = NA_real_,
          final_max = NA_real_,
          lims_all = y_all_first
        )
      }
    }

    ratio_val <- (x_span_ratio / y_span_ratio) * aspect_ratio

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
#' steps, limits, expansion, and aspect ratio. This is a single-plot analogue
#' of `align_axis_scales()`.
#'
#' The function treats each axis independently and is careful about discrete
#' vs continuous axes:
#'
#' * If an axis is continuous (numeric), it will:
#'   - derive an automatic range from the data,
#'   - merge that with any user-supplied `*_limits`,
#'   - optionally "round up" the max via `expand_breaks = TRUE` so that
#'     the final span is an exact multiple of `*_break_step`,
#'   - generate breaks with `seq(min, max, by = *_break_step)`,
#'   - apply `scale_*_continuous(expand = expand)`,
#'   - enforce visible range via `coord_cartesian()` / `coord_fixed()`.
#'
#' * If an axis is discrete (factor / character), that axis is LEFT UNTOUCHED:
#'   - we do not call `floor()` / `ceiling()` on it,
#'   - we do not add `scale_*_continuous()` for it,
#'   - we do not set `xlim` / `ylim` for that axis in `coord_*()`.
#'   If you pass `x_limits` or `y_limits` for a discrete axis, they will be
#'   ignored with a warning.
#'
#' Separation of concerns:
#'
#' * `x_limits`, `y_limits` (merged with automatic data ranges) define the
#'   *visible* numeric range, via `coord_cartesian(xlim=..., ylim=...)`
#'   or `coord_fixed()`.
#'
#' * `expand` controls padding at the panel edges by being passed to
#'   `scale_x_continuous(expand = ...)` / `scale_y_continuous(expand = ...)`.
#'   This is visual gap, not data range.
#'
#' * `expand_breaks = TRUE` can slightly extend the computed max range so that
#'   the span is an exact multiple of `x_break_step` / `y_break_step`.
#'   This makes tick marks "clean" (e.g. 10, 15, 20, 25, 30). It only applies
#'   to continuous axes.
#'
#' @param plot A ggplot object.
#' @param x_break_step Numeric. Step size for x-axis breaks (default: 3).
#' @param y_break_step Numeric. Step size for y-axis breaks (default: 3).
#' @param x_limits Optional numeric vector. x-axis visible limits, either
#'   `c(min, max)` or a single number. Only used if x is continuous.
#' @param y_limits Optional numeric vector. y-axis visible limits, either
#'   `c(min, max)` or a single number. Only used if y is continuous.
#' @param aspect_ratio Optional numeric. If specified, enforces a fixed aspect
#'   ratio via `coord_fixed()`. If one axis is discrete, the ratio is computed
#'   using the available continuous axis and a fallback of 1 for the discrete
#'   axis; a warning is issued.
#' @param clip Character. Passed to `coord_*()`. One of `"on"`, `"off"`,
#'   or `"inherit"` (default: `"off"`).
#' @param expand_breaks Logical. If `TRUE`, adjusts the computed max (and
#'   therefore the break sequence and coord limits) so the span is an exact
#'   multiple of the break step. Only applies to continuous axes.
#' @param expand Expansion specification for `scale_x_continuous()` and
#'   `scale_y_continuous()`. Typical values:
#'   - `waiver()` (default): ggplot2 default ~5% padding.
#'   - `expansion(mult = 0)`: no padding at panel edges.
#'   - `expansion(add = c(0, 2))`: add fixed headroom on the high end, etc.
#'
#' @return A ggplot object with adjusted axis scales.
#'
#' @examples
#' library(ggplot2)
#'
#' # A scatterplot (both axes continuous) ------------------------------------
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   labs(title = "Miles per Gallon vs Weight")
#'
#' # 1. Basic usage with custom breaks
#' p_basic <- adjust_axis_scales(
#'   plot = p,
#'   x_break_step = 0.5,
#'   y_break_step = 5
#' )
#' p_basic
#'
#' # 2. Force visible ranges using x_limits / y_limits
#' #    Breaks are then seq(min, max, by = step) within those forced limits.
#' p_forced_limits <- adjust_axis_scales(
#'   plot = p,
#'   x_limits = c(2, 5),
#'   y_limits = c(10, 35),
#'   x_break_step = 0.5,
#'   y_break_step = 5
#' )
#' p_forced_limits
#'
#' # 3. Make tick spacing "pretty" with expand_breaks = TRUE
#' #    The function will round up the max so the span is a clean multiple
#' #    of the break step (continuous axes only).
#' p_pretty_ticks <- adjust_axis_scales(
#'   plot = p,
#'   x_break_step = 1,
#'   y_break_step = 5,
#'   expand_breaks = TRUE
#' )
#' p_pretty_ticks
#'
#' # 4. Control panel padding with `expand`
#' #    (A) default padding (~5%), (B) none at all, (C) extra headroom.
#' p_default_pad <- adjust_axis_scales(
#'   plot = p,
#'   x_break_step = 0.5,
#'   y_break_step = 5,
#'   expand = waiver()
#' )
#'
#' p_no_pad <- adjust_axis_scales(
#'   plot = p,
#'   x_break_step = 0.5,
#'   y_break_step = 5,
#'   expand = expansion(mult = 0)
#' )
#'
#' p_headroom <- adjust_axis_scales(
#'   plot = p,
#'   x_break_step = 0.5,
#'   y_break_step = 5,
#'   expand = expansion(add = c(0, 2))  # +2 on the high end of y
#' )
#'
#' # 5. Lock aspect ratio so that 1 unit of x ~ 1 unit of y visually
#' p_fixed_ratio <- adjust_axis_scales(
#'   plot = p,
#'   x_break_step = 0.5,
#'   y_break_step = 5,
#'   aspect_ratio = 1
#' )
#' p_fixed_ratio
#'
#'
#' # Boxplot example (discrete x, continuous y) ------------------------------
#' p_box <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
#'   geom_boxplot(fill = "gray70") +
#'   labs(
#'     title = "MPG by cylinder count",
#'     x = "cyl",
#'     y = "mpg"
#'   )
#'
#' # 6. For boxplots, x is discrete. We only adjust y.
#' #    - y_limits fixes visible mpg range.
#' #    - y_break_step controls tick spacing.
#' #    - expand_breaks = TRUE rounds up the top to a clean multiple of 5.
#' #    - expand = expansion(mult = 0) removes top/bottom padding so boxes
#' #      sit flush against the panel border.
#' p_box_forced <- adjust_axis_scales(
#'   plot = p_box,
#'   y_limits = c(10, 35),
#'   y_break_step = 5,
#'   expand_breaks = TRUE,
#'   expand = expansion(mult = 0)
#' )
#' p_box_forced
#'
#' # 7. Add headroom above the whiskers for annotations without changing
#' #    the discrete x. We just pad y using `expand = expansion(add = c(0, 2))`.
#' p_box_headroom <- adjust_axis_scales(
#'   plot = p_box,
#'   y_limits = c(10, 35),
#'   y_break_step = 5,
#'   expand = expansion(add = c(0, 2))
#' )
#' p_box_headroom
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

  x_is_num <- is.numeric(x_limits_all)
  y_is_num <- is.numeric(y_limits_all)

  # x-axis calculations (only if continuous) --------------------------------
  if (x_is_num) {
    xmin <- floor(min(x_limits_all, na.rm = TRUE))
    xmax <- ceiling(max(x_limits_all, na.rm = TRUE))

    x_new <- merge_lim(xmin, xmax, x_limits)
    xmin <- x_new[1]
    xmax <- x_new[2]

    if (expand_breaks) {
      x_span <- xmax - xmin
      x_rem <- x_span %% x_break_step
      if (!isTRUE(all.equal(x_rem, 0))) {
        xmax <- xmax + (x_break_step - x_rem)
      }
    }
  } else {
    xmin <- xmax <- NULL
    if (!is.null(x_limits)) {
      warning("x axis appears to be discrete; ignoring `x_limits`.")
    }
  }

  # y-axis calculations (only if continuous) --------------------------------
  if (y_is_num) {
    ymin <- floor(min(y_limits_all, na.rm = TRUE))
    ymax <- ceiling(max(y_limits_all, na.rm = TRUE))

    y_new <- merge_lim(ymin, ymax, y_limits)
    ymin <- y_new[1]
    ymax <- y_new[2]

    if (expand_breaks) {
      y_span <- ymax - ymin
      y_rem <- y_span %% y_break_step
      if (!isTRUE(all.equal(y_rem, 0))) {
        ymax <- ymax + (y_break_step - y_rem)
      }
    }
  } else {
    ymin <- ymax <- NULL
    if (!is.null(y_limits)) {
      warning("y axis appears to be discrete; ignoring `y_limits`.")
    }
  }

  # Scales with custom breaks and expansion ---------------------------------
  shared_scales <- list()

  if (x_is_num) {
    shared_scales <- c(
      shared_scales,
      scale_x_continuous(
        breaks = seq(xmin, xmax, by = x_break_step),
        expand = expand
      )
    )
  }

  if (y_is_num) {
    shared_scales <- c(
      shared_scales,
      scale_y_continuous(
        breaks = seq(ymin, ymax, by = y_break_step),
        expand = expand
      )
    )
  }

  # Coordinate system with visible limits -----------------------------------
  coord_args <- list(clip = clip)

  if (x_is_num) {
    coord_args$xlim <- c(xmin, xmax)
  }
  if (y_is_num) {
    coord_args$ylim <- c(ymin, ymax)
  }

  # aspect_ratio handling ---------------------------------------------------
  coord_obj <- if (is.null(aspect_ratio)) {
    do.call(coord_cartesian, coord_args)
  } else {
    # Compute span for x and y, using numeric range if continuous,
    # or factor->as.numeric() if discrete.
    x_span_ratio <- axis_span_for_ratio(
      is_num = x_is_num,
      final_min = if (x_is_num) xmin else NA_real_,
      final_max = if (x_is_num) xmax else NA_real_,
      lims_all = x_limits_all
    )

    y_span_ratio <- axis_span_for_ratio(
      is_num = y_is_num,
      final_min = if (y_is_num) ymin else NA_real_,
      final_max = if (y_is_num) ymax else NA_real_,
      lims_all = y_limits_all
    )

    ratio_val <- (x_span_ratio / y_span_ratio) * aspect_ratio

    do.call(coord_fixed, c(coord_args, list(ratio = ratio_val)))
  }

  shared_scales <- c(shared_scales, coord_obj)

  plot + shared_scales
}

#' @keywords internal
axis_span_for_ratio <- function(is_num, final_min, final_max, lims_all) {
  if (is_num) {
    span <- abs(final_max - final_min)
    if (span == 0) {
      span <- 1
    }
    return(span)
  } else {
    lev_num <- as.numeric(factor(lims_all, levels = unique(lims_all)))
    rng <- range(lev_num, na.rm = TRUE)
    span <- abs(rng[2] - rng[1]) + 0.6 * 2
    if (span == 0) {
      span <- 1
    }
    return(span)
  }
}
