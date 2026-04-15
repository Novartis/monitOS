# Getting Started with monitOS

## What is monitOS?

**monitOS** provides statistical guidelines for monitoring overall
survival (OS) in pivotal clinical trials for **indolent cancers** —
slow-progressing diseases (e.g. follicular lymphoma, CLL, low-grade
glioma) where experimental therapies are often evaluated against a
watch-and-wait or standard-of-care control.

The core problem: in indolent cancers, OS benefit is hard to prove in a
reasonable timeframe, yet OS harm must not be obscured by surrogate
endpoints. The package implements the framework of [Fleming et
al. (2024)](https://www.tandfonline.com/doi/full/10.1080/19466315.2024.2365648),
which defines a sequence of **positivity thresholds** — OS hazard ratio
(HR) boundaries that must be met at each planned analysis to give
sufficient reassurance that the experimental treatment does not cause
unacceptable harm.

> **Key convention**: HR \< 1 indicates benefit; HR ≥ 1 indicates
> potential harm.

------------------------------------------------------------------------

## Core parameters

Three clinically-determined ingredients drive every monitoring plan:

| Parameter | Argument  | Meaning                                                                              |
|-----------|-----------|--------------------------------------------------------------------------------------|
| δ_null    | `hr_null` | The OS HR that is **unacceptably harmful** (e.g. 1.3 = 30% excess mortality)         |
| δ_alt     | `hr_alt`  | The OS HR that is **plausibly beneficial** given the mechanism of action (e.g. 0.80) |
| Events    | `events`  | Cumulative OS deaths at each planned analysis                                        |

Two error-rate parameters complete the design:

| Parameter | Argument    | Meaning                                                                             |
|-----------|-------------|-------------------------------------------------------------------------------------|
| γ_FA      | `falsepos`  | One-sided false positive rate tolerated at the **Final Analysis** (typically 0.025) |
| β_PA      | `power_int` | Statistical power at the **Primary (interim) Analysis** under δ_alt (typically 0.9) |

------------------------------------------------------------------------

## Installation

``` r
install.packages("monitOS")
```

``` r
library(monitOS)
```

------------------------------------------------------------------------

## Scenario 1 — Standard trial: Follicular Lymphoma

### Clinical context

A Phase 3 randomised trial evaluates a novel PI3Kδ inhibitor combined
with standard immunochemotherapy (R-CHOP) versus R-CHOP alone in
previously untreated Grade 1–2 follicular lymphoma — a classic indolent
B-cell malignancy with a long natural history.

- **Randomisation**: 1:1 (experimental : control)  
- **δ_null = 1.30** — a 30% increase in mortality risk is deemed
  unacceptable for a population where watch-and-wait is a safe
  alternative  
- **δ_alt = 0.80** — a 20% reduction in mortality risk is plausible
  based on the mechanism and Phase 2 PFS data  
- **5 analyses** planned at 50, 80, 110, 140, and 175 OS events  
- **γ_FA = 0.025**, **β_PA = 0.90**

``` r
fl_trial <- bounds(
  events     = c(50, 80, 110, 140, 175),
  power_int  = 0.90,   # 90% power at primary analysis under δ_alt
  falsepos   = 0.025,  # one-sided α at final analysis
  hr_null    = 1.30,   # unacceptable OS harm threshold
  hr_alt     = 0.80,   # plausible OS benefit
  rand_ratio = 1       # 1:1 randomisation
)

fl_trial$summary
#>   Deaths OS HR threshold for positivity One-sided false positive error rate
#> 1     50                          1.150                               0.332
#> 2     80                          1.065                               0.187
#> 3    110                          1.021                               0.103
#> 4    140                          0.993                               0.056
#> 5    175                          0.967                               0.025
#>   Level of 2-sided CI needed to rule out delta null
#> 1                                                34
#> 2                                                63
#> 3                                                79
#> 4                                                89
#> 5                                                95
#>   Probability of meeting positivity threshold under delta alt
#> 1                                                       0.900
#> 2                                                       0.900
#> 3                                                       0.900
#> 4                                                       0.900
#> 5                                                       0.895
#>   Posterior probability the true OS HR exceeds delta null given the data
#> 1                                                                  0.332
#> 2                                                                  0.187
#> 3                                                                  0.103
#> 4                                                                  0.056
#> 5                                                                  0.025
#>   Predictive probability the OS HR estimate at Final Analysis does not exceed the positivity threshold
#> 1                                                                                               23.425
#> 2                                                                                               27.725
#> 3                                                                                               31.742
#> 4                                                                                               35.834
#> 5                                                                                                   NA
```

### Interpreting the output

Each row corresponds to one planned analysis. The key columns are:

- **OS HR threshold for positivity**: if the observed OS HR at that
  analysis is *at or below* this value, the treatment is considered
  reassuring (it does not appear to cause unacceptable harm). Notice
  thresholds tighten as evidence accumulates — from a lenient ~1.13 at
  50 events down to ~0.97 at the Final Analysis.

- **One-sided false positive error rate**: the cumulative Type I error
  spent at each analysis. Only the Final Analysis exhausts the full γ_FA
  = 0.025.

- **Posterior probability HR ≥ δ_null**: the Bayesian probability that
  the *true* OS HR exceeds the unacceptable threshold, given the
  observed estimate equals the positivity boundary — a useful
  decision-support metric alongside the threshold itself.

- **Predictive probability (%)**: at each interim, the probability that
  the Final Analysis will also meet its positivity threshold, *assuming
  the current trend continues*. This helps early stopping discussions.

------------------------------------------------------------------------

## Scenario 2 — Edge case: CLL Maintenance with Unequal Randomisation

### Clinical context

A Phase 3 trial evaluates a BCL-2 inhibitor maintenance strategy versus
observation in patients with chronic lymphocytic leukaemia (CLL) who
achieved response to first-line chemo-immunotherapy.

This scenario introduces two complications common in real drug
development:

1.  **2:1 randomisation** — more patients are allocated to the
    experimental arm to accelerate safety and biomarker data collection.
2.  **Marginal benefit sensitivity analysis** — the sponsor wishes to
    understand monitoring performance under a *pessimistic* scenario
    where the drug provides only a modest 5% reduction in mortality (HR
    = 0.95), well below the expected HR = 0.80 used to size the trial.

- **δ_null = 1.25** — a tighter margin than Scenario 1 because CLL
  patients already have a relatively good prognosis; even modest harm is
  unacceptable  
- **δ_alt = 0.80** — 20% OS improvement anticipated based on PFS phase 2
  data  
- **5 analyses** at 60, 95, 130, 165, and 200 OS events  
- **hr_marg_benefit = 0.95** — the marginal benefit scenario to
  stress-test

``` r
cll_trial <- bounds(
  events          = c(60, 95, 130, 165, 200),
  power_int       = 0.90,
  falsepos        = 0.025,
  hr_null         = 1.25,   # tighter harm margin for CLL
  hr_alt          = 0.80,
  rand_ratio      = 2,      # 2:1 experimental : control
  hr_marg_benefit = 0.95    # sensitivity: only 5% OS benefit
)

cll_trial$summary
#>   Deaths OS HR threshold for positivity One-sided false positive error rate
#> 1     60                          1.136                               0.364
#> 2     95                          1.057                               0.221
#> 3    130                          1.015                               0.132
#> 4    165                          0.989                               0.078
#> 5    200                          0.932                               0.025
#>   Level of 2-sided CI needed to rule out delta null
#> 1                                                27
#> 2                                                56
#> 3                                                74
#> 4                                                84
#> 5                                                95
#>   Probability of meeting positivity threshold under delta alt
#> 1                                                       0.900
#> 2                                                       0.900
#> 3                                                       0.900
#> 4                                                       0.900
#> 5                                                       0.845
#>   Posterior probability the true OS HR exceeds delta null given the data
#> 1                                                                  0.356
#> 2                                                                  0.207
#> 3                                                                  0.118
#> 4                                                                  0.066
#> 5                                                                  0.019
#>   Predictive probability the OS HR estimate at Final Analysis does not exceed the positivity threshold
#> 1                                                                                               17.887
#> 2                                                                                               19.719
#> 3                                                                                               20.324
#> 4                                                                                               18.110
#> 5                                                                                                   NA
#>   Probability of meeting positivity threshold under incremental benefit
#> 1                                                                 0.743
#> 2                                                                 0.689
#> 3                                                                 0.640
#> 4                                                                 0.595
#> 5                                                                 0.448
```

### Key observations for the edge case

**Effect of unequal randomisation**: The 2:1 allocation reduces the
Fisher information per event compared to 1:1, requiring slightly higher
observed thresholds (more room for the HR to be above 1 while still
passing). This is reflected in the positivity thresholds being
fractionally higher than in Scenario 1 at equivalent event counts.

**Marginal benefit column**: The rightmost column shows the probability
of meeting each positivity threshold if the true OS HR is only 0.95 — a
scenario where the drug barely works. Even under this pessimistic
assumption the monitoring thresholds are likely to be met at most
analyses, providing reassurance that the monitoring plan does not
require large OS benefit to conclude “no unacceptable harm.”

------------------------------------------------------------------------

## Quick reference

``` r
# Minimal call — all other arguments have sensible defaults
bounds(events = c(60, 100, 150))

# Full call
bounds(
  events          = c(60, 89, 110, 131, 178),
  power_int       = 0.90,
  falsepos        = 0.025,
  hr_null         = 1.30,
  hr_alt          = 0.80,
  rand_ratio      = 1,
  hr_marg_benefit = NULL   # omit or set to a scalar HR for sensitivity analysis
)
```

For an interactive version of these analyses, visit the [Shiny
app](https://opensource.nibr.com/monitOS/shinylive-app/) — no R
installation required.
