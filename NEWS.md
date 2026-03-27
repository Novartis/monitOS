# monitOS 0.2.0

- Added Shinylive (WebAssembly) version of Shiny app for browser-only use.
- Added new pkgdown article "Interactive App (Shinylive)" with embedded app.
- Enhanced `_pkgdown.yml` with custom navbar, article sections, and reference grouping.
- Improved roxygen2 documentation for `app_server()` and `app_ui()`.
- Fixed `app_ui()` typo: `with = 12` corrected to `width = 12`.
- Fixed incomplete paper URL in Shiny app (now links to published paper).
- Modernized `calc_predictive()` and `meeting_probs()` to use `vapply()` instead of for-loops.
- Added explicit `return()` in `calc_predictive()`.
- Removed dead commented-out code from `app_server()` and `run_app()`.
- Documented hardcoded false positive rate in `find_pos()`.
- Aligned `.lintr` line width (80) with `air.toml` formatter config.
- Added new tests for `calc_posterior()`, `calc_predictive()`, and `meeting_probs()`.
- Updated GitHub Actions: `actions/checkout@v4`, added shinylive export step.
- Added `shinylive` to `Suggests`.

# monitOS 0.1.6

- Changed `Licence` to `MIT`.
- Added spell checking to package.
- Added github issues link `BugReport` and website link to `URL` in `Description.
- Fix import issue with `shiny` dependency.
- Fix bug returning `inf` values in Shiny app.
- Added new `badges` to packages

# monitOS 0.1.5

-   Improved `DESCRIPTION`, added website URL, screenshot Shiny app
-   Improved `README`
-   Improved code style using `styler`
-   Fixed bug deploying `Shiny` app (relative import error)
-   Fixed package coverage by focusing only on statistical tool (no shiny app)

# monitOS 0.1.4

-   Added a `NEWS.md` file to track changes to the package.
-   Added `Shiny` section in `README.md` file.
-   Improved `monitOS::run_app()`: new layout, new descriptions, new use case.
-   Created `pkgdown` public website.

# monitOS 0.1.3

-   Initial CRAN submission.
