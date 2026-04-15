# monitOS

`monitOS` helps clinical teams define **positivity thresholds** for
monitoring overall survival (OS) in pivotal trials for indolent cancers.

It turns a small set of trial assumptions into a table of operating
characteristics you can use at interim and final analyses:

- the **unacceptable detrimental effect** on OS you want to rule out
  (`hr_null`)
- the **plausible beneficial effect** on OS you still consider realistic
  (`hr_alt`)
- the **deaths expected at each analysis** (`events`)

The framework is described in the [monitOS
paper](https://www.tandfonline.com/doi/full/10.1080/19466315.2024.2365648).

## Install and launch

Install from CRAN:

``` r
install.packages("monitOS")
```

Load the package:

``` r
library(monitOS)
```

## Use the app

The fastest way to explore the method is through the interactive app.

- 🌐 **[Try it online
  (Shinylive)](https://opensource.nibr.com/monitOS/shinylive-app/)** —
  runs entirely in your browser via WebAssembly, no installation
  required.
- 💻 **Run locally** with R:

``` r
monitOS::run_app()
```

## What the inputs mean

`monitOS` is driven by a few clinically meaningful choices.

| Concept                            | Argument          | What it means                                                              |
|------------------------------------|-------------------|----------------------------------------------------------------------------|
| Unacceptable OS detriment          | `hr_null`         | The smallest OS HR that would already be too harmful to accept             |
| Plausible OS benefit               | `hr_alt`          | The OS HR that still represents a realistic beneficial effect              |
| Planned deaths                     | `events`          | Cumulative deaths expected at each analysis                                |
| Final-analysis false positive rate | `falsepos`        | One-sided error rate tolerated at the final analysis                       |
| Primary-analysis power             | `power_int`       | Required power at the primary analysis when the true OS HR equals `hr_alt` |
| Randomization ratio                | `rand_ratio`      | Experimental-to-control allocation ratio                                   |
| Incremental-benefit scenario       | `hr_marg_benefit` | Optional weaker-benefit scenario used for sensitivity analysis             |

Key convention: **OS HR \< 1 indicates benefit**.

## Quick start

This minimal example creates positivity thresholds for three planned
analyses:

``` r
library(monitOS)

plan <- bounds(
  events = c(60, 110, 178),
  power_int = 0.90,
  falsepos = 0.025,
  hr_null = 1.30,
  hr_alt = 0.80
)

plan$summary
```

What to look for:

- `OS HR threshold for positivity` tells you the largest observed OS HR
  that still provides enough reassurance at each analysis.
- `One-sided false positive error rate` shows the tolerated error at
  each stage.
- `Level of 2-sided CI needed to rule out delta null` tells you the
  confidence level needed to exclude the unacceptable detriment.

## Example 1: Standard 1:1 design

``` r
library(monitOS)

fl_trial <- bounds(
  events = c(50, 80, 110, 140, 175),
  power_int = 0.90,
  falsepos = 0.025,
  hr_null = 1.30,
  hr_alt = 0.80,
  rand_ratio = 1
)

fl_trial$summary
```

Interpretation: as evidence accumulates, the positivity threshold
typically becomes more stringent. Early analyses may tolerate an
observed OS HR slightly above 1, while the final analysis usually
requires a threshold below 1.

## Example 2: Unequal randomization with sensitivity analysis

``` r
library(monitOS)

cll_trial <- bounds(
  events = c(60, 95, 130, 165, 200),
  power_int = 0.90,
  falsepos = 0.025,
  hr_null = 1.25,
  hr_alt = 0.80,
  rand_ratio = 2,
  hr_marg_benefit = 0.95
)

cll_trial$summary
```

This adds an incremental-benefit column showing how often the monitoring
thresholds would still be met if the true OS effect were more modest
than the main alternative.

## Need help?

- Package vignette:
  <https://opensource.nibr.com/monitOS/articles/monitOS.html>
- Interactive app: <https://opensource.nibr.com/monitOS/shinylive-app/>
- Function reference:

``` r
?bounds
?run_app
```

- Paper:
  <https://www.tandfonline.com/doi/full/10.1080/19466315.2024.2365648>
- Issues and questions: <https://github.com/Novartis/monitOS/issues>
