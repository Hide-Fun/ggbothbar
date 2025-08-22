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
#' This function calculates the isotopic enrichment factors (e13c and e15n) by comparing
#' sample values to a reference group.
#'
#' @param data A data frame containing isotopic data
#' @param var Character string specifying the column name that distinguishes between reference and sample groups
#' @param delta Character vector of length 2 specifying the column names for d13C and d15N values
#' @param reference Character string specifying the reference group value in the 'var' column
#' @param na.rm Logical; if TRUE, removes NA values when calculating mean reference values
#'
#' @return A data frame with additional columns:
#'   \item{e13c}{Enrichment factor for carbon isotopes (d13C sample - mean d13C reference)}
#'   \item{e15n}{Enrichment factor for nitrogen isotopes (d15N sample - mean d15N reference)}
#'
#' @examples
#' # Create example data
#' df <- data.frame(
#'   type = c("reference", "reference", "sample", "sample"),
#'   d13c = c(-20, -21, -18, -19),
#'   d15n = c(5, 6, 8, 9)
#' )
#'
#' # Calculate enrichment
#' enriched_data <- calc_enrichment(df, delta = c("d13c", "d15n"))
#'
#' @export
calc_enrichment <- function(data,
                            var = "type",
                            delta = c("d13C", "d15N"), # add new argument
                            reference = "reference",
                            na.rm = FALSE) {
  # split data
  reference_data <- data[data[[var]] == reference, ]

  # calculate mean and sd
  mean_ref_1 <- mean(reference_data[[delta[1]]], na.rm = na.rm)
  mean_ref_2 <- mean(reference_data[[delta[2]]], na.rm = na.rm)

  # calculate enrichment factor
  e13c <- data[[delta[1]]] - mean_ref_1
  e15n <- data[[delta[2]]] - mean_ref_2

  result <- data
  result$e13c <- e13c
  result$e15n <- e15n

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
#' fix_limit(p, .ratio = 1)
#'
#' @importFrom ggplot2 ggplot_build coord_fixed
#' @export
fix_limit <- function(.plot, .ratio, .clip = "off") {
  # get x limits
  xr1 <- ggplot_build(.plot)$layout$panel_scales_x[[1]]$range$range[[1]]
  xr2 <- ggplot_build(.plot)$layout$panel_scales_x[[1]]$range$range[[2]]
  # get y limits
  yr1 <- ggplot_build(.plot)$layout$panel_scales_y[[1]]$range$range[[1]]
  yr2 <- ggplot_build(.plot)$layout$panel_scales_y[[1]]$range$range[[2]]
  # calculate diff
  x_range <- abs(xr1 - xr2)
  y_range <- abs(yr1 - yr2)
  stund <- x_range / y_range
  rlt <- .plot + coord_fixed(ratio = stund * .ratio, clip = .clip)
  return(rlt)
}

#' Create formatted axis labels for isotope data
#'
#' @param mass_number Numeric. The isotope number (e.g., 13 for carbon-13)
#' @param element Character. The element symbol (e.g., "C" for carbon)
#' @param notation Character. Either "delta" or "epsilon" (default: "delta")
#' @param units Character. The units to display (default: "‰")
#'
#' @return An expression object for use in ggplot2 axis labels
#' @export
#'
#' @examples
#' # For delta 13C
#' label_isotope(13, "C")
#' # For epsilon 15N
#' label_isotope(15, "N", notation = "epsilon")
label_isotope <- function(mass_number, element, notation = "delta", units = "‰") {
  # Validate inputs
  if (!is.numeric(mass_number)) {
    stop("mass_number must be numeric")
  }

  if (!is.character(element) || nchar(element) == 0) {
    stop("element must be a non-empty character string")
  }

  if (!(notation %in% c("delta", "epsilon"))) {
    stop('notation must be either "delta" or "epsilon"')
  }

  # Define the notation symbol
  symbol <- switch(notation,
    "delta" = "δ",
    "epsilon" = "ε"
  )

  # Create the expression using bquote() for proper evaluation
  result <- bquote(
    expression(
      paste(
        italic(.(symbol)^.(mass_number)),
        .(element),
        " (", .(units), ")"
      )
    )
  )

  # Evaluate the expression
  return(eval(result))
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
#'
#' @return A tibble with:
#' \itemize{
#'   \item spreadsheet_id: The ID of the created Google spreadsheet (if local=FALSE)
#'   \item spreadsheet_url: The URL of the created Google spreadsheet (if local=FALSE)
#'   \item file_path: The local path where the Excel file was saved (if local=TRUE or download=TRUE)
#' }
#'
#' @importFrom googlesheets4 gs4_create sheet_names sheet_rename sheet_write sheet_add gs4_has_token gs4_auth
#' @importFrom googledrive drive_find drive_download drive_has_token drive_auth
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
    path = NULL # File path for local save or download
    ) {
  # Validate dependencies
  assert_dependencies(local, download)

  # Validate parameters
  assert_parameters(.data, sheet_names, name, local, download)

  # Set file path if not provided
  if (is.null(path)) {
    path <- file.path(getwd(), paste0(name, ".xlsx"))
  }

  # Choose the appropriate method based on parameters
  if (local) {
    return(write_local_excel(.data, sheet_names, path))
  } else {
    result <- write_google_sheets(.data, sheet_names, name)

    # Download if requested
    if (download) {
      file_path <- download_google_sheet(name, path)
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

  missing_pkgs <- required_pkgs[!purrr::map_lgl(required_pkgs, ~ requireNamespace(.x, quietly = TRUE))]

  if (length(missing_pkgs) > 0) {
    pkg_list <- paste0("'", missing_pkgs, "'", collapse = ", ")
    install_cmd <- paste0("install.packages(c(", pkg_list, "))")
    stop("Required packages missing: ", pkg_list, ". Please install with: ", install_cmd)
  }
}

#' Validate function parameters
#'
#' @param .data List of dataframes
#' @param sheet_names Vector of sheet names
#' @param name Spreadsheet/file name
#' @param local Local Excel option
#' @param download Download option
#' @keywords internal
assert_parameters <- function(.data, sheet_names, name, local, download) {
  # Check data input
  if (!is.list(.data)) {
    stop("'.data' must be a list of dataframes", call. = FALSE)
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

  if (length(sheet_names) != length(.data)) {
    stop(
      "'sheet_names' must have the same length as '.data' (",
      length(.data), " vs ", length(sheet_names), ")",
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
  if (!is.logical(local) || length(local) != 1) {
    stop("'local' must be a logical value (TRUE or FALSE)", call. = FALSE)
  }

  if (!is.logical(download) || length(download) != 1) {
    stop("'download' must be a logical value (TRUE or FALSE)", call. = FALSE)
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
#' @return A tibble with the file path
#' @keywords internal
write_local_excel <- function(data_list, sheet_names, file_path) {
  message("Saving directly to local Excel file: ", file_path)

  # Create a new workbook
  wb <- openxlsx::createWorkbook()

  # Add sheets and write data
  purrr::walk2(
    data_list, sheet_names,
    ~ {
      message("Adding sheet: ", .y)
      openxlsx::addWorksheet(wb, .y)
      openxlsx::writeData(wb, sheet = .y, .x)
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
#' @return A tibble with spreadsheet ID and URL
#' @keywords internal
write_google_sheets <- function(data_list, sheet_names, spreadsheet_name) {
  # Check authentication
  if (!googlesheets4::gs4_has_token()) {
    message("Authenticating with Google Sheets...")
    googlesheets4::gs4_auth()
  }

  # Create a new Google spreadsheet
  message("Creating new spreadsheet: ", spreadsheet_name)
  ss <- googlesheets4::gs4_create(spreadsheet_name)

  # Get spreadsheet URL
  ss_url <- as.character(ss)
  message("Spreadsheet URL: https://docs.google.com/spreadsheets/d/", ss_url)

  # Get current sheet names and rename the first sheet
  current_sheets <- googlesheets4::sheet_names(ss)
  googlesheets4::sheet_rename(ss, sheet = current_sheets[1], new_name = sheet_names[1])

  # Write the first dataframe to the first sheet
  message("Writing data to sheet: ", sheet_names[1])
  googlesheets4::sheet_write(data_list[[1]], ss, sheet = sheet_names[1])

  # Add remaining sheets and write dataframes
  if (length(data_list) > 1) {
    purrr::walk2(
      data_list[-1], sheet_names[-1],
      ~ {
        message("Adding sheet: ", .y)
        googlesheets4::sheet_add(ss, .y)

        message("Writing data to sheet: ", .y)
        googlesheets4::sheet_write(.x, ss, sheet = .y)
      }
    )
  }

  message("Google Sheets spreadsheet created successfully")
  return(dplyr::tibble(
    spreadsheet_id = ss,
    spreadsheet_url = ss_url
  ))
}

#' Download a Google Spreadsheet as Excel
#'
#' @param spreadsheet_name Name of the spreadsheet to download
#' @param file_path Path to save the Excel file
#' @return The file path if successful, NULL otherwise
#' @keywords internal
download_google_sheet <- function(spreadsheet_name, file_path) {
  if (!googledrive::drive_has_token()) {
    message("Authenticating with Google Drive...")
    googledrive::drive_auth()
  }

  # Find the file in Google Drive
  message("Finding spreadsheet in Google Drive...")
  file <- googledrive::drive_find(pattern = spreadsheet_name, type = "spreadsheet")

  if (nrow(file) == 0) {
    warning("Spreadsheet not found in Google Drive. Could not download file.", call. = FALSE)
    return(NULL)
  }

  # Create directory if it doesn't exist
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path) && dir_path != ".") {
    dir.create(dir_path, recursive = TRUE)
  }

  # Download the file
  message("Downloading spreadsheet to: ", file_path)
  googledrive::drive_download(
    file = file$id[1], # Use the first match if multiple files found
    path = file_path,
    type = "xlsx",
    overwrite = TRUE
  )

  message("Download completed successfully")
  return(file_path)
}
