# Changelog

## monitOS 0.2.0

- Added Shinylive (WebAssembly) version of Shiny app for browser-only
  use.
- Added new pkgdown article “Interactive App (Shinylive)” with embedded
  app.
- Enhanced `_pkgdown.yml` with custom navbar, article sections, and
  reference grouping.
- Improved roxygen2 documentation for
  [`app_server()`](https://opensource.nibr.com/monitOS/reference/app_server.md)
  and
  [`app_ui()`](https://opensource.nibr.com/monitOS/reference/app_ui.md).
- Fixed
  [`app_ui()`](https://opensource.nibr.com/monitOS/reference/app_ui.md)
  typo: `with = 12` corrected to `width = 12`.
- Fixed incomplete paper URL in Shiny app (now links to published
  paper).
- Modernized
  [`calc_predictive()`](https://opensource.nibr.com/monitOS/reference/calc_predictive.md)
  and
  [`meeting_probs()`](https://opensource.nibr.com/monitOS/reference/meeting_probs.md)
  to use [`vapply()`](https://rdrr.io/r/base/lapply.html) instead of
  for-loops.
- Added explicit [`return()`](https://rdrr.io/r/base/function.html) in
  [`calc_predictive()`](https://opensource.nibr.com/monitOS/reference/calc_predictive.md).
- Removed dead commented-out code from
  [`app_server()`](https://opensource.nibr.com/monitOS/reference/app_server.md)
  and
  [`run_app()`](https://opensource.nibr.com/monitOS/reference/run_app.md).
- Documented hardcoded false positive rate in
  [`find_pos()`](https://opensource.nibr.com/monitOS/reference/find_pos.md).
- Aligned `.lintr` line width (80) with `air.toml` formatter config.
- Added new tests for
  [`calc_posterior()`](https://opensource.nibr.com/monitOS/reference/calc_posterior.md),
  [`calc_predictive()`](https://opensource.nibr.com/monitOS/reference/calc_predictive.md),
  and
  [`meeting_probs()`](https://opensource.nibr.com/monitOS/reference/meeting_probs.md).
- Updated GitHub Actions: `actions/checkout@v4`, added shinylive export
  step.
- Added `shinylive` to `Suggests`.

## monitOS 0.1.6

CRAN release: 2025-07-17

- Changed `Licence` to `MIT`.
- Added spell checking to package.
- Added github issues link `BugReport` and website link to `URL` in
  \`Description.
- Fix import issue with `shiny` dependency.
- Fix bug returning `inf` values in Shiny app.
- Added new `badges` to packages

## monitOS 0.1.5

CRAN release: 2024-03-21

- Improved `DESCRIPTION`, added website URL, screenshot Shiny app
- Improved `README`
- Improved code style using `styler`
- Fixed bug deploying `Shiny` app (relative import error)
- Fixed package coverage by focusing only on statistical tool (no shiny
  app)

## monitOS 0.1.4

CRAN release: 2024-03-05

- Added a `NEWS.md` file to track changes to the package.
- Added `Shiny` section in `README.md` file.
- Improved
  [`monitOS::run_app()`](https://opensource.nibr.com/monitOS/reference/run_app.md):
  new layout, new descriptions, new use case.
- Created `pkgdown` public website.

## monitOS 0.1.3

CRAN release: 2023-10-31

- Initial CRAN submission.
