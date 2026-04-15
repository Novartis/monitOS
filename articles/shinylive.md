# Interactive App (Shinylive)

## Try monitOS in your browser

The monitOS Shiny app is available as a **Shinylive** application that
runs entirely in your browser using WebAssembly — no R installation or
server required.

The interactive app requires JavaScript. Please enable JavaScript in
your browser or visit the [standalone
app](https://opensource.nibr.com/monitOS/shinylive-app/) directly.

## About Shinylive

[Shinylive](https://shinylive.io) runs Shiny applications entirely in
the browser using [WebAssembly](https://webassembly.org/) via the
[webR](https://docs.r-wasm.org/webr/latest/) project. This means: - **No
server required** — the app runs on your device - **No installation
needed** — just open the page - **Privacy** — your inputs never leave
your browser

The app implements the same
[`bounds()`](https://opensource.nibr.com/monitOS/reference/bounds.md)
function available in the R package. For programmatic access, install
monitOS from CRAN:

``` r
install.packages("monitOS")
library(monitOS)
bounds(
  events = c(60, 89, 110, 131, 178),
  power_int = 0.9,
  falsepos = 0.025,
  hr_null = 1.3,
  hr_alt = 0.8,
  rand_ratio = 1
)
```

## Local Shiny App

If you have R and monitOS installed, you can also run the full Shiny
dashboard locally:

``` r
monitOS::run_app()
```
