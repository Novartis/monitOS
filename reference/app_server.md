# Shiny app server function

Server-side logic for the monitOS Shiny application. Computes OS
monitoring boundaries reactively based on user inputs and renders the
summary table of positivity thresholds.

## Usage

``` r
app_server(input, output, session)
```

## Arguments

- input:

  Shiny input object containing UI widget values.

- output:

  Shiny output object for rendering results.

- session:

  Shiny session object for updating UI elements.
