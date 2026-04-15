# Package index

## OS Monitoring Boundaries

Core statistical functions for computing positivity thresholds.

- [`bounds()`](https://opensource.nibr.com/monitOS/reference/bounds.md)
  : Bounds
- [`find_pos()`](https://opensource.nibr.com/monitOS/reference/find_pos.md)
  : Find Positivity Threshold

## Bayesian Metrics

Posterior and predictive probability calculations.

- [`calc_posterior()`](https://opensource.nibr.com/monitOS/reference/calc_posterior.md)
  : Function which calculates for k=1, ..., K, Pr(log-HR \>= lhr_null \|
  theta.hat.k = lhr_con.k)
- [`calc_predictive()`](https://opensource.nibr.com/monitOS/reference/calc_predictive.md)
  : Calculate posterior predictive probability of ruling out lhr_null at
  final OS analysis
- [`meeting_probs()`](https://opensource.nibr.com/monitOS/reference/meeting_probs.md)
  : Probabilities of meeting positivity threshold under target HR

## Shiny App

Interactive Shiny dashboard for OS monitoring.

- [`run_app()`](https://opensource.nibr.com/monitOS/reference/run_app.md)
  : monitOS app
- [`app_server()`](https://opensource.nibr.com/monitOS/reference/app_server.md)
  : Shiny app server function
- [`app_ui()`](https://opensource.nibr.com/monitOS/reference/app_ui.md)
  : Shiny app UI definition
