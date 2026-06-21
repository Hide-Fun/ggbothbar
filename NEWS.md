# ggbothbar 1.1.1

## New features

- Added `stat_mean_label()` for placing labels at group mean positions.
- Added `theme_aca()` as the preferred academic plotting theme name; `theme_isotope()` remains available for backward compatibility.
- Added `fix_aspect_ratio()` as the preferred aspect-ratio helper name; `fix_limit()` remains available for backward compatibility.
- Added formatted spreadsheet output options to `write_sheets()`: header filters, first-row freezing, and automatic column widths for local xlsx and Google Sheets exports.

## Improvements

- Improved `write_sheets()` input validation for empty data lists, invalid logical options, duplicate sheet names, long sheet names, and Excel-incompatible sheet names.
- Changed Google Sheets downloads to use the newly created spreadsheet ID instead of name-based Drive search, avoiding accidental downloads of similarly named files.
- Improved `fix_aspect_ratio()` error messages for discrete axes, empty plots, zero-width ranges, and invalid `.ratio` values.
- Made `label_isotope()` use Unicode escapes internally while preserving rendered isotope labels.

## Bug fixes

- Avoided empty panel output in errorbar grob rendering.
- Made undefined confidence intervals in `calc_error(fun.errorbar = "ci")` return `NA_real_` instead of producing `NaN` warnings.
- Added a warning when `stat_mean_label()` receives multiple labels within one group while preserving the existing first-label behavior.

## Maintenance

- Added repository agent instructions and kept generated Rd files synchronized with `devtools::document()`.

# ggbothbar 1.0.0

## Breaking changes

- Deprecated `draw_reference_box()` in favor of `geom_errorbox()` (#4)

## New features

- Added `geom_errorbox()` for enhanced error box visualization (#4)
- Added `fix_aspect_ratio()` function to adjust plot aspect ratios based on plot dimensions; `fix_limit()` remains available for backward compatibility (#6)
- Added customizable theme system `theme_aca()`; `theme_isotope()` remains available for backward compatibility
- Added `delta` parameter to `calc_enrichment()` for flexible isotope column specification (#5)
- Added `label_isotope()` for creating labels (e.g., axis title) of isotope plot.
- Added `write_sheets()` for writting multiple data.frame into one Excel sheet / Google Spreadsheet.

## Improvements

- Enhanced documentation with more detailed examples
- Standardized theme settings with customizable parameters
