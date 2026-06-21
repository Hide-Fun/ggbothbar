#' Calculate Standard Error
#'
#' This function calculates the standard error of a numeric vector.
#'
#' @param x A numeric vector
#' @param na.rm A logical value indicating whether NA values should be stripped before the computation proceeds. Defaults to FALSE
#' @return The standard error of the input vector
#' @examples
#' se(c(1, 2, 3, 4, 5))
#' se(c(1, 2, 3, 4, 5, NA), na.rm = TRUE)
#' @export
se <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- stats::na.omit(x)
  }
  stats::sd(x) / sqrt(length(x))
}


#' Calculate Error for Errorbars
#'
#' This function calculates an error value (standard deviation, standard error,
#' 95% confidence interval, or a user-supplied function) for a numeric vector.
#'
#' @param x A numeric vector
#' @param fun.errorbar Either a character string specifying the method to
#'   calculate the error ("sd", "se", or "ci"), or a function that accepts
#'   a numeric vector and returns a single numeric value.
#' @param na.rm A logical value indicating whether \code{NA} values should be
#'   removed before computation. Defaults to \code{FALSE}.
#' @return The calculated error value of the input vector
#' @examples
#' calc_error(c(1, 2, 3, 4, 5))
#' calc_error(c(1, 2, 3, 4, 5, NA), fun.errorbar = "se", na.rm = TRUE)
#' calc_error(c(1, 2, 3, 4, 5), fun.errorbar = "ci")
#' calc_error(c(1, 2, 3, 4, 5), fun.errorbar = function(x) max(x) - min(x))
#' @export
calc_error <- function(x, fun.errorbar = "sd", na.rm = FALSE) {
  if (is.character(fun.errorbar)) {
    if (fun.errorbar == "sd") {
      stats::sd(x, na.rm = na.rm)
    } else if (fun.errorbar == "se") {
      se(x, na.rm = na.rm)
    } else if (fun.errorbar == "ci") {
      n <- if (na.rm) sum(!is.na(x)) else length(x)
      se(x, na.rm = na.rm) * stats::qt(0.975, df = n - 1)
    } else {
      stop("Unsupported fun.errorbar: ", fun.errorbar)
    }
  } else if (is.function(fun.errorbar)) {
    fun.errorbar(x)
  } else {
    stop("fun.errorbar must be a character string or function")
  }
}

#' Calculate Isotopic Enrichment
#'
#' Calculate isotopic enrichment factors by subtracting mean reference values
#' from each supplied isotope column.
#'
#' @param data A data frame containing isotopic data
#' @param var Character string specifying the column name that distinguishes between reference and sample groups
#' @param delta Character vector specifying the column names that store \eqn{\delta} values (e.g., d13C, d15N, d34S)
#' @param epsilon Optional character vector specifying the names of the enrichment columns to append; defaults to swapping the leading
#'   \code{"d"} in each entry of \code{delta} for \code{"e"} when \code{delta} matches the pattern \code{^d\\d+[A-Za-z]+$}
#' @param reference Character string specifying the reference group value in the 'var' column
#' @param na.rm Logical; if TRUE, removes NA values when calculating mean reference values
#'
#' @return A data frame with additional enrichment columns, one for each element of \code{delta}
#'
#' @examples
#' # Example data
#' df <- data.frame(
#'   type = c("reference", "sample", "sample", "reference"),
#'   d13C = c(-20.0, -21.5, -19.0, -20.5),
#'   d15N = c(7.0, 8.2, 6.5, 7.4),
#'   d34S = c(12.0, 11.5, 13.2, 12.4)
#' )
#'
#' # 1) Default: epsilon names are inferred as "e13C" and "e15N" (backword compatibility)
#' out1 <- calc_enrichment(df)
#' head(out1)
#'
#' # 2) User supplied output names
#' out2 <- calc_enrichment(
#'   df,
#'   delta = c("d13C", "d15N"),
#'   epsilon = c("e13c", "e15n")
#' )
#' head(out2)
#'
#' # 3) Multiple delta columns with inferred epsilon names ("e13C", "e15N", "e34S")
#' out3 <- calc_enrichment(
#'   df,
#'   delta = c("d13C", "d15N", "d34S")
#' )
#' head(out3)
#'
#' # 4) Multiple delta columns with explicit epsilon names
#' out4 <- calc_enrichment(
#'   df,
#'   delta = c("d13C", "d15N", "d34S"),
#'   epsilon = c("E13C_enr", "E15N_enr", "E34S_enr")
#' )
#' head(out4)
#'
#' @export
calc_enrichment <- function(
  data,
  var = "type",
  delta = c("d13C", "d15N"),
  epsilon = NULL,
  reference = "reference",
  na.rm = FALSE
) {
  # -- Validation -------------------------------------------------------------
  # Check that `var` column exists
  if (!is.character(var) || length(var) != 1L || !(var %in% names(data))) {
    stop("`var` must be the name of a column in `data`.")
  }

  # Check that `reference` level exists
  if (!is.character(reference) || length(reference) != 1L) {
    stop("`reference` must be a single character value.")
  }
  reference_rows <- data[[var]] == reference
  if (!any(reference_rows, na.rm = TRUE)) {
    stop("No rows match `reference` in the `var` column.")
  }

  # Check `delta`
  if (!is.character(delta) || length(delta) < 1L) {
    stop("`delta` must be a character vector of length >= 1.")
  }
  missing_delta <- setdiff(delta, names(data))
  if (length(missing_delta) > 0L) {
    stop(sprintf(
      "These `delta` columns are missing in `data`: %s",
      paste(missing_delta, collapse = ", ")
    ))
  }

  # Derive or validate `epsilon`
  if (is.null(epsilon)) {
    # When epsilon is NULL, validate delta patterns and derive names
    # Pattern: start with 'd', followed by digits, then one or more letters.
    is_valid <- grepl("^d\\d+[A-Za-z]+$", delta, perl = TRUE)
    if (!all(is_valid)) {
      bad <- delta[!is_valid]
      stop(sprintf(
        "When `epsilon` is NULL, each `delta` must match pattern ^d\\d+[A-Za-z]+$. Invalid: %s",
        paste(bad, collapse = ", ")
      ))
    }
    epsilon <- sub("^d", "e", delta)
  } else {
    if (!is.character(epsilon) || length(epsilon) != length(delta)) {
      stop("`epsilon` must be a character vector the same length as `delta`.")
    }
  }

  # -- Computation ------------------------------------------------------------
  reference_data <- data[reference_rows, , drop = FALSE]

  # Compute reference means for each delta
  ref_means <- vapply(
    delta,
    function(col) mean(reference_data[[col]], na.rm = na.rm),
    numeric(1)
  )

  # Prepare result and append enrichment columns
  result <- data
  for (i in seq_along(delta)) {
    # Enrichment = sample value - reference mean
    result[[epsilon[i]]] <- data[[delta[i]]] - ref_means[i]
  }

  return(result)
}

#' Fix Aspect Ratio of ggplot Based on Plot Limits
#'
#' This function adjusts the aspect ratio of a ggplot object by calculating the ratio
#' based on the current plot limits and a desired ratio modifier.
#'
#' @param .plot A ggplot object
#' @param .ratio Numeric value to modify the calculated aspect ratio
#' @param .clip Character string specifying the clipping behavior ("off" by default)
#'   See \code{\link[ggplot2]{coord_fixed}} for more details
#'
#' @return A modified ggplot object with adjusted aspect ratio
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' # Adjust the aspect ratio
#' fix_aspect_ratio(p, .ratio = 1)
#'
#' @importFrom ggplot2 ggplot_build coord_fixed
#' @export
fix_aspect_ratio <- function(.plot, .ratio, .clip = "off") {
  if (!is.numeric(.ratio) || length(.ratio) != 1L || !is.finite(.ratio) || .ratio <= 0) {
    stop("'.ratio' must be a single positive finite numeric value", call. = FALSE)
  }

  built_plot <- ggplot_build(.plot)
  x_range_raw <- built_plot$layout$panel_scales_x[[1]]$range$range
  y_range_raw <- built_plot$layout$panel_scales_y[[1]]$range$range

  validate_axis_range <- function(range, axis) {
    if (!is.numeric(range) || length(range) != 2L || any(!is.finite(range))) {
      stop(
        "Cannot fix aspect ratio because the ",
        axis,
        " axis range is not a finite numeric range.",
        call. = FALSE
      )
    }

    axis_range <- abs(range[[1]] - range[[2]])
    if (axis_range <= 0) {
      stop(
        "Cannot fix aspect ratio because the ",
        axis,
        " axis range has zero width.",
        call. = FALSE
      )
    }

    axis_range
  }

  # calculate diff
  x_range <- validate_axis_range(x_range_raw, "x")
  y_range <- validate_axis_range(y_range_raw, "y")
  stund <- x_range / y_range
  rlt <- .plot + coord_fixed(ratio = stund * .ratio, clip = .clip)
  return(rlt)
}

#' Fix Aspect Ratio of ggplot Based on Plot Limits
#'
#' `fix_limit()` is kept for backward compatibility. Use
#' `fix_aspect_ratio()` for new code.
#'
#' @inheritParams fix_aspect_ratio
#'
#' @return A modified ggplot object with adjusted aspect ratio
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' # Backward-compatible API
#' fix_limit(p, .ratio = 1)
#'
#' @export
fix_limit <- function(.plot, .ratio, .clip = "off") {
  fix_aspect_ratio(.plot = .plot, .ratio = .ratio, .clip = .clip)
}

#' Create formatted axis labels for isotope data
#'
#' @param mass_number Numeric. The isotope number (e.g., 13 for carbon-13)
#' @param element Character. The element symbol (e.g., "C" for carbon)
#' @param notation Character. Either "delta" or "epsilon" (default: "delta")
#' @param units Character. The units to display (default: "\\u2030")
#' @param italic_iso_symbol Logical. Should the delta/epsilon symbol be set in italics? (default: FALSE)
#' @param is_markdown Logical. If TRUE, output in markdown format compatible with the marquee package
#'        (uses `{.sup ...}` for superscript). If FALSE (default), output as expression for ggplot2.
#'
#' @return If is_markdown = FALSE, returns an expression object for ggplot2 axis labels.
#'         If is_markdown = TRUE, returns a character string using marquee-style markdown,
#'         e.g., `\\u03b4{.sup13}C (\\u2030)` or `*\\u03b5*{.sup15}N (\\u2030)` depending on italic_iso_symbol.
#' @export
#'
#' @examples
#' # For delta 13C (expression)
#' label_isotope(13, "C")
#' # For epsilon 15N (markdown, symbol italic)
#' label_isotope(15, "N", notation = "epsilon", italic_iso_symbol = TRUE, is_markdown = TRUE)
label_isotope <- function(
  mass_number,
  element,
  notation = "delta",
  units = "\u2030",
  italic_iso_symbol = FALSE,
  is_markdown = FALSE
) {
  # Validate inputs
  if (!is.numeric(mass_number) || length(mass_number) != 1) {
    stop("mass_number must be a single numeric value")
  }
  if (!is.character(element) || length(element) != 1 || nchar(element) == 0) {
    stop("element must be a non-empty single character string")
  }
  if (!(notation %in% c("delta", "epsilon"))) {
    stop('notation must be either "delta" or "epsilon"')
  }
  if (!is.logical(italic_iso_symbol) || length(italic_iso_symbol) != 1) {
    stop("italic_iso_symbol must be a single logical value")
  }
  if (!is.logical(is_markdown) || length(is_markdown) != 1) {
    stop("is_markdown must be a single logical value")
  }

  # Define the notation symbol
  symbol <- switch(notation, "delta" = "\u03b4", "epsilon" = "\u03b5")

  if (is_markdown) {
    # Markdown output using marquee style custom span for superscript
    # Format: optionally italic symbol, then symbol{.sup mass_number}element (units)
    symbol_md <- if (italic_iso_symbol) {
      paste0("*", symbol, "*")
    } else {
      symbol
    }
    label_md <- paste0(
      symbol_md,
      "{.sup ",
      mass_number,
      "}",
      element,
      " (",
      units,
      ")"
    )
    return(label_md)
  } else {
    # Expression output for ggplot2
    symbol_expr <- if (italic_iso_symbol) {
      bquote(italic(.(symbol)))
    } else {
      bquote(.(symbol))
    }
    result <- bquote(
      expression(
        paste(
          .(symbol_expr)^.(mass_number),
          .(element),
          " (",
          .(units),
          ")"
        )
      )
    )
    return(eval(result))
  }
}

#' Write Multiple Dataframes to Google Sheets or Local Excel File
#'
#' Takes multiple dataframes and writes them to separate sheets in either:
#' 1. A new Google Spreadsheet (default)
#' 2. A local Excel file (when local = TRUE)
#'
#' When using Google Sheets (option 1), you can optionally download
#' the spreadsheet as an Excel file (when download = TRUE)
#'
#' @param .data A list of dataframes to save
#' @param sheet_names A character vector of sheet names (must match length of `.data`)
#' @param name A string specifying the name of the spreadsheet/file to create
#' @param local Logical. Whether to save directly to a local Excel file. Default is FALSE.
#' @param download Logical. Whether to download the Google spreadsheet as an Excel file. Default is FALSE.
#' @param path Optional. Path where the Excel file will be saved.
#'            If NULL (default), saves to working directory with `name`.
#' @param filter Logical. Whether to enable column filters on the header row.
#' @param freeze_first_row Logical. Whether to freeze the first row.
#' @param auto_width Logical. Whether to automatically fit column widths.
#'
#' @return A tibble with:
#' \itemize{
#'   \item spreadsheet_id: The ID of the created Google spreadsheet (if local=FALSE)
#'   \item spreadsheet_url: The URL of the created Google spreadsheet (if local=FALSE)
#'   \item file_path: The local path where the Excel file was saved (if local=TRUE or download=TRUE)
#' }
#'
#' @importFrom googlesheets4 gs4_create sheet_names sheet_rename sheet_write sheet_add gs4_has_token gs4_auth gs4_get as_sheets_id request_generate request_make range_autofit
#' @importFrom googledrive as_id drive_download drive_has_token drive_auth
#' @importFrom dplyr tibble
#' @importFrom rlang .data .env
#' @importFrom purrr map map_lgl
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Create sample dataframes
#' student_data <- tibble(
#'   id = 1:5,
#'   name = c("John", "Emma", "David", "Sarah", "Michael"),
#'   score = c(85, 92, 78, 95, 88)
#' )
#'
#' product_data <- tibble(
#'   product_id = 101:105,
#'   product_name = c("Laptop", "Smartphone", "Tablet", "Earphones", "Mouse"),
#'   price = c(850, 950, 600, 150, 50)
#' )
#'
#' # Basic usage - write to Google Sheets
#' list(student_data, product_data) %>%
#'   write_sheets(
#'     sheet_names = c("Students", "Products"),
#'     name = "Sample_Data"
#'   )
#'
#' # Write to local Excel file
#' list(student_data, product_data) %>%
#'   write_sheets(
#'     sheet_names = c("Students", "Products"),
#'     name = "Sample_Data",
#'     local = TRUE
#'   )
#'
#' # Write to Google Sheets and download
#' result <- list(student_data, product_data) %>%
#'   write_sheets(
#'     sheet_names = c("Students", "Products"),
#'     name = "Sample_Data",
#'     download = TRUE
#'   )
#'
#' # Access the spreadsheet URL
#' result$spreadsheet_url
#' }
#'
#' @export
write_sheets <- function(
  .data, # List of dataframes to save
  sheet_names, # List of sheet names
  name, # Name of the spreadsheet or file
  local = FALSE, # Whether to save directly to local Excel
  download = FALSE, # Whether to download the Google spreadsheet
  path = NULL, # File path for local save or download
  filter = TRUE, # Whether to add filters to the header row
  freeze_first_row = TRUE, # Whether to freeze the first row
  auto_width = TRUE # Whether to auto-fit column widths
) {
  # Validate parameters
  assert_parameters(
    .data,
    sheet_names,
    name,
    local,
    download,
    filter,
    freeze_first_row,
    auto_width
  )

  # Validate dependencies
  assert_dependencies(local, download)

  # Set file path if not provided
  if (is.null(path)) {
    path <- file.path(getwd(), paste0(name, ".xlsx"))
  }

  # Choose the appropriate method based on parameters
  if (local) {
    return(write_local_excel(
      .data,
      sheet_names,
      path,
      filter = filter,
      freeze_first_row = freeze_first_row,
      auto_width = auto_width
    ))
  } else {
    result <- write_google_sheets(
      .data,
      sheet_names,
      name,
      filter = filter,
      freeze_first_row = freeze_first_row,
      auto_width = auto_width
    )

    # Download if requested
    if (download) {
      file_path <- download_google_sheet(result$spreadsheet_id[[1]], path)
      if (!is.null(file_path)) {
        result$file_path <- file_path
      }
    }

    return(result)
  }
}

#' Assert that required packages are available
#'
#' @param local Whether local Excel saving is requested
#' @param download Whether downloading is requested
#' @keywords internal
assert_dependencies <- function(local, download) {
  required_pkgs <- character()

  if (!local) {
    required_pkgs <- c(required_pkgs, "googlesheets4")
  }

  if (download) {
    required_pkgs <- c(required_pkgs, "googledrive")
  }

  if (local) {
    required_pkgs <- c(required_pkgs, "openxlsx")
  }

  required_pkgs <- c(required_pkgs, "dplyr")

  missing_pkgs <- required_pkgs[
    !purrr::map_lgl(required_pkgs, ~ requireNamespace(.x, quietly = TRUE))
  ]

  if (length(missing_pkgs) > 0) {
    pkg_list <- paste0("'", missing_pkgs, "'", collapse = ", ")
    install_cmd <- paste0("install.packages(c(", pkg_list, "))")
    stop(
      "Required packages missing: ",
      pkg_list,
      ". Please install with: ",
      install_cmd
    )
  }
}

#' Validate function parameters
#'
#' @param .data List of dataframes
#' @param sheet_names Vector of sheet names
#' @param name Spreadsheet/file name
#' @param local Local Excel option
#' @param download Download option
#' @param filter Filter option
#' @param freeze_first_row Freeze first row option
#' @param auto_width Auto-width option
#' @keywords internal
assert_parameters <- function(
  .data,
  sheet_names,
  name,
  local,
  download,
  filter = TRUE,
  freeze_first_row = TRUE,
  auto_width = TRUE
) {
  # Check data input
  if (!is.list(.data)) {
    stop("'.data' must be a list of dataframes", call. = FALSE)
  }

  if (length(.data) < 1L) {
    stop("'.data' must contain at least one dataframe", call. = FALSE)
  }

  # Check each element is a data frame
  non_df <- purrr::map_lgl(.data, ~ !is.data.frame(.x))
  if (any(non_df)) {
    stop("All elements in '.data' must be dataframes", call. = FALSE)
  }

  # Check sheet names
  if (!is.character(sheet_names)) {
    stop("'sheet_names' must be a character vector", call. = FALSE)
  }

  if (anyNA(sheet_names) || any(!nzchar(sheet_names))) {
    stop("'sheet_names' must not contain missing or empty values", call. = FALSE)
  }

  if (length(sheet_names) != length(.data)) {
    stop(
      "'sheet_names' must have the same length as '.data' (",
      length(.data),
      " vs ",
      length(sheet_names),
      ")",
      call. = FALSE
    )
  }

  if (anyDuplicated(tolower(sheet_names))) {
    stop("'sheet_names' must be unique, ignoring case", call. = FALSE)
  }

  too_long <- nchar(sheet_names) > 31L
  if (any(too_long)) {
    stop(
      "'sheet_names' must be 31 characters or fewer for Excel compatibility: ",
      paste(sheet_names[too_long], collapse = ", "),
      call. = FALSE
    )
  }

  invalid_chars <- c(":", "\\", "/", "?", "*", "[", "]")
  invalid_names <- vapply(
    strsplit(sheet_names, "", fixed = TRUE),
    function(chars) any(chars %in% invalid_chars),
    logical(1)
  )
  if (any(invalid_names)) {
    stop(
      "'sheet_names' must not contain Excel-invalid characters (: \\ / ? * [ ]): ",
      paste(sheet_names[invalid_names], collapse = ", "),
      call. = FALSE
    )
  }

  # Check name
  if (!is.character(name) || length(name) != 1) {
    stop("'name' must be a single string", call. = FALSE)
  }

  if (nchar(name) == 0) {
    stop("'name' cannot be an empty string", call. = FALSE)
  }

  # Check boolean parameters
  if (!is.logical(local) || length(local) != 1 || is.na(local)) {
    stop("'local' must be a logical value (TRUE or FALSE)", call. = FALSE)
  }

  if (!is.logical(download) || length(download) != 1 || is.na(download)) {
    stop("'download' must be a logical value (TRUE or FALSE)", call. = FALSE)
  }

  if (!is.logical(filter) || length(filter) != 1 || is.na(filter)) {
    stop("'filter' must be a logical value (TRUE or FALSE)", call. = FALSE)
  }

  if (
    !is.logical(freeze_first_row) ||
      length(freeze_first_row) != 1 ||
      is.na(freeze_first_row)
  ) {
    stop(
      "'freeze_first_row' must be a logical value (TRUE or FALSE)",
      call. = FALSE
    )
  }

  if (!is.logical(auto_width) || length(auto_width) != 1 || is.na(auto_width)) {
    stop("'auto_width' must be a logical value (TRUE or FALSE)", call. = FALSE)
  }

  # Special case handling
  if (local && download) {
    warning(
      "Both 'local' and 'download' are TRUE. ",
      "Will save locally without using Google Sheets.",
      call. = FALSE
    )
  }
}

#' Write dataframes to a local Excel file
#'
#' @param data_list List of dataframes
#' @param sheet_names Vector of sheet names
#' @param file_path Path to save the Excel file
#' @param filter Logical. Whether to enable column filters on the header row
#' @param freeze_first_row Logical. Whether to freeze the first row
#' @param auto_width Logical. Whether to automatically fit column widths
#' @return A tibble with the file path
#' @keywords internal
write_local_excel <- function(
  data_list,
  sheet_names,
  file_path,
  filter = TRUE,
  freeze_first_row = TRUE,
  auto_width = TRUE
) {
  message("Saving directly to local Excel file: ", file_path)

  # Create a new workbook
  wb <- openxlsx::createWorkbook()

  # Add sheets and write data
  purrr::walk2(
    data_list,
    sheet_names,
    ~ {
      message("Adding sheet: ", .y)
      openxlsx::addWorksheet(wb, .y)
      openxlsx::writeData(wb, sheet = .y, .x)

      if (filter && ncol(.x) > 0) {
        openxlsx::addFilter(wb, sheet = .y, rows = 1, cols = seq_len(ncol(.x)))
      }

      if (freeze_first_row) {
        openxlsx::freezePane(wb, sheet = .y, firstRow = TRUE)
      }

      if (auto_width && ncol(.x) > 0) {
        openxlsx::setColWidths(
          wb,
          sheet = .y,
          cols = seq_len(ncol(.x)),
          widths = "auto"
        )
      }
    }
  )

  # Create directory if it doesn't exist
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path) && dir_path != ".") {
    dir.create(dir_path, recursive = TRUE)
  }

  # Save the workbook
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

  message("Local Excel file saved successfully")
  return(dplyr::tibble(file_path = file_path))
}

#' Write dataframes to Google Sheets
#'
#' @param data_list List of dataframes
#' @param sheet_names Vector of sheet names
#' @param spreadsheet_name Name of the spreadsheet
#' @param filter Logical. Whether to enable column filters on the header row
#' @param freeze_first_row Logical. Whether to freeze the first row
#' @param auto_width Logical. Whether to automatically fit column widths
#' @return A tibble with spreadsheet ID and URL
#' @keywords internal
write_google_sheets <- function(
  data_list,
  sheet_names,
  spreadsheet_name,
  filter = TRUE,
  freeze_first_row = TRUE,
  auto_width = TRUE
) {
  # Check authentication
  if (!googlesheets4::gs4_has_token()) {
    message("Authenticating with Google Sheets...")
    googlesheets4::gs4_auth()
  }

  # Create a new Google spreadsheet
  message("Creating new spreadsheet: ", spreadsheet_name)
  ss <- googlesheets4::gs4_create(spreadsheet_name)

  # Get spreadsheet URL
  ss_id <- as.character(ss)
  ss_url <- paste0("https://docs.google.com/spreadsheets/d/", ss_id)
  message("Spreadsheet URL: ", ss_url)

  # Get current sheet names and rename the first sheet
  current_sheets <- googlesheets4::sheet_names(ss)
  googlesheets4::sheet_rename(
    ss,
    sheet = current_sheets[1],
    new_name = sheet_names[1]
  )

  # Write the first dataframe to the first sheet
  message("Writing data to sheet: ", sheet_names[1])
  googlesheets4::sheet_write(data_list[[1]], ss, sheet = sheet_names[1])
  apply_google_sheet_format(
    ss,
    sheet_names[1],
    data_list[[1]],
    filter = filter,
    freeze_first_row = freeze_first_row,
    auto_width = auto_width
  )

  # Add remaining sheets and write dataframes
  if (length(data_list) > 1) {
    purrr::walk2(
      data_list[-1],
      sheet_names[-1],
      ~ {
        message("Adding sheet: ", .y)
        googlesheets4::sheet_add(ss, .y)

        message("Writing data to sheet: ", .y)
        googlesheets4::sheet_write(.x, ss, sheet = .y)
        apply_google_sheet_format(
          ss,
          .y,
          .x,
          filter = filter,
          freeze_first_row = freeze_first_row,
          auto_width = auto_width
        )
      }
    )
  }

  message("Google Sheets spreadsheet created successfully")
  return(dplyr::tibble(
    spreadsheet_id = ss_id,
    spreadsheet_url = ss_url
  ))
}

#' Apply spreadsheet formatting to a Google Sheet worksheet
#'
#' @param ss Google Sheet identifier
#' @param sheet_name Worksheet name
#' @param data Dataframe written to the worksheet
#' @param filter Logical. Whether to enable column filters on the header row
#' @param freeze_first_row Logical. Whether to freeze the first row
#' @param auto_width Logical. Whether to automatically fit column widths
#' @return The input Google Sheet identifier, invisibly
#' @keywords internal
apply_google_sheet_format <- function(
  ss,
  sheet_name,
  data,
  filter = TRUE,
  freeze_first_row = TRUE,
  auto_width = TRUE
) {
  if (ncol(data) == 0) {
    return(invisible(ss))
  }

  ss_id <- googlesheets4::as_sheets_id(ss)
  ss_meta <- googlesheets4::gs4_get(ss_id)
  sheet_info <- ss_meta$sheets[ss_meta$sheets$name == sheet_name, , drop = FALSE]

  if (nrow(sheet_info) != 1) {
    stop("Could not identify Google Sheet worksheet: ", sheet_name, call. = FALSE)
  }

  sheet_id <- sheet_info$id[[1]]
  requests <- list()

  if (filter) {
    requests <- append(requests, list(list(
      setBasicFilter = list(
        filter = list(
          range = list(
            sheetId = sheet_id,
            startRowIndex = 0,
            endRowIndex = nrow(data) + 1,
            startColumnIndex = 0,
            endColumnIndex = ncol(data)
          )
        )
      )
    )))
  }

  if (freeze_first_row) {
    requests <- append(requests, list(list(
      updateSheetProperties = list(
        properties = list(
          sheetId = sheet_id,
          gridProperties = list(frozenRowCount = 1)
        ),
        fields = "gridProperties.frozenRowCount"
      )
    )))
  }

  if (length(requests) > 0) {
    req <- googlesheets4::request_generate(
      "sheets.spreadsheets.batchUpdate",
      params = list(spreadsheetId = ss_id, requests = requests)
    )
    googlesheets4::request_make(req)
  }

  if (auto_width) {
    googlesheets4::range_autofit(ss_id, sheet = sheet_name, dimension = "columns")
  }

  invisible(ss_id)
}

#' Download a Google Spreadsheet as Excel
#'
#' @param spreadsheet_id ID of the spreadsheet to download
#' @param file_path Path to save the Excel file
#' @return The file path if successful, NULL otherwise
#' @keywords internal
download_google_sheet <- function(spreadsheet_id, file_path) {
  if (!googledrive::drive_has_token()) {
    message("Authenticating with Google Drive...")
    googledrive::drive_auth()
  }

  # Create directory if it doesn't exist
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path) && dir_path != ".") {
    dir.create(dir_path, recursive = TRUE)
  }

  # Download the file
  message("Downloading spreadsheet to: ", file_path)
  googledrive::drive_download(
    file = googledrive::as_id(spreadsheet_id),
    path = file_path,
    type = "xlsx",
    overwrite = TRUE
  )

  message("Download completed successfully")
  return(file_path)
}
