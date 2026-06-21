# ggbothbar 1.0.0

## Breaking changes

- Deprecated `draw_reference_box()` in favor of `geom_errorbox()` (#4)

## New features

- Added `geom_errorbox()` for enhanced error box visualization (#4)
- Added `fix_limit()` function to adjust plot aspect ratios based on plot dimensions (#6)
- Added customizable theme system `theme_aca()`; `theme_isotope()` remains available for backward compatibility
- Added `delta` parameter to `calc_enrichment()` for flexible isotope column specification (#5)
- Added `label_isotope()` for creating labels (e.g., axis title) of isotope plot.
- Added `write_sheets()` for writting multiple data.frame into one Excel sheet / Google Spreadsheet.

## Improvements

- Enhanced documentation with more detailed examples
- Standardized theme settings with customizable parameters
