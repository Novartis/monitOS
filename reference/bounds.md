# Bounds

OS monitoring guidelines as proposed in manuscript "Monitoring Overall
Survival in Pivotal Trials in Indolent Cancers". Calculate thresholds
for positivity that can be used at an analysis to judge whether emerging
evidence about the effect of treatment on OS is concerning or not. The
threshold for positivity at any given analysis is the value below which
the observed hazard ratio must be in order to provide sufficient
reassurance that the effect on OS does not reach the selected
unacceptable level of detriment (the margin hr_null). Terminology
follows the manuscript "Monitoring Overall Survival in Pivotal Trials in
Indolent Cancers"

## Usage

``` r
bounds(
  events,
  power_int = 0.9,
  falsepos = 0.025,
  hr_null = 1.3,
  hr_alt = 0.9,
  rand_ratio = 1,
  hr_marg_benefit = NULL
)
```

## Arguments

- events:

  Vector. Target number of deaths at each analysis

- power_int:

  Scalar. Marginal power required at the Primary Analysis when true
  hazard ratio (HR) is hr_alt.

- falsepos:

  Scalar. Marginal one-sided false positive error rate we are prepared
  to tolerate at the Final Analysis. Determines the positivity threshold
  at Final Analysis

- hr_null:

  Scalar. The unacceptably large detrimental effect of treatment on OS
  we want to rule out (on HR scale)

- hr_alt:

  Scalar. Plausible clinically relevant beneficial effect of treatment
  on OS (on HR scale)

- rand_ratio:

  Integer. If patients are randomized k:1 between experimental
  intervention and control, rand_ratio should be inputted as k. Example:
  if patients are randomized 1:1 between experimental and control, k=1.
  If patients are randomized 2:1 between experimental and control, k=2.

- hr_marg_benefit:

  Scalar. We may be uncertain about what a plausible beneficial effect
  of treatment on OS is. User can enter a (on HR scale) and function
  will evaluate the probability we meet the positivity threshold at each
  analysis under this HR. This second OS benefit will usually be closer
  to 1 than hr_alt.

## Value

List that contains:

- `lhr_null`: Scalar, unacceptable OS log-HR,

- `lhr_alt`: Scalar, plausible clinically relevant log-HR,

- `lhr_pos`: Scalar, positivity thresholds for log-HR estimates,

- `summary`: Dataframe, which contains:

  - `OS HR threshold for positivity`,

  - `One sided false positive error rate`,

  - `Level of 2 sided CI needed to rule out hr_null`,

  - `Probability of meeting positivity threshold under hr_alt`,

  - `Positivity_Thres_Posterior`: Pr(true OS HR \>= minimum unacceptable
    OS HR \| current data),

  - `Positivity_Thres_PredProb`: Pr(OS HR estimate at Final Analysis \<=
    Final Analysis positivity threshold \| current data)

## Details

Monitoring guidelines assume that the hazard ratio (HR) can adequately
summarize the size of the benefits and harms of the experimental
intervention vs control on overall survival (OS). Furthermore,
guidelines assume that an OS HR \< 1 is consistent with a beneficial
effect of the intervention on OS (and smaller OS HRs \<1 indicate
increased efficacy).

## Examples

``` r
# Example 01: OS monitoring guideline retrospectively applied to Motivating Example 1
# with delta null = 1.3, delta alt = 0.80, gamma_FA = 0.025 and  beta_PA = 0.10.
bounds(
  events = c(60, 89, 110, 131, 178),
  power_int = 0.9, # beta_PA
  falsepos = 0.025, # gamma_FA
  hr_null = 1.3, # delta_null
  hr_alt = 0.8, # delta_alt
  rand_ratio = 1, # rand_ratio
  hr_marg_benefit = NULL
)
#> $lhr_null
#> [1] 0.2623643
#> 
#> $lhr_alt
#> [1] -0.2231436
#> 
#> $lhr_pos
#> [1]  0.107751640  0.048544837  0.021238743  0.000795809 -0.031446759
#> 
#> $summary
#>   Deaths OS HR threshold for positivity One-sided false positive error rate
#> 1     60                          1.114                               0.275
#> 2     89                          1.050                               0.157
#> 3    110                          1.021                               0.103
#> 4    131                          1.001                               0.067
#> 5    178                          0.969                               0.025
#>   Level of 2-sided CI needed to rule out delta null
#> 1                                                45
#> 2                                                69
#> 3                                                79
#> 4                                                87
#> 5                                                95
#>   Probability of meeting positivity threshold under delta alt
#> 1                                                         0.9
#> 2                                                         0.9
#> 3                                                         0.9
#> 4                                                         0.9
#> 5                                                         0.9
#>   Posterior probability the true OS HR exceeds delta null given the data
#> 1                                                                  0.275
#> 2                                                                  0.157
#> 3                                                                  0.103
#> 4                                                                  0.067
#> 5                                                                  0.025
#>   Predictive probability the OS HR estimate at Final Analysis does not exceed the positivity threshold
#> 1                                                                                               25.394
#> 2                                                                                               29.681
#> 3                                                                                               32.744
#> 4                                                                                               35.977
#> 5                                                                                                   NA
#> 
# Example 02: OS monitoring guideline applied to Motivating Example 2
# with delta null = 4/3, delta alt = 0.7, gamma_FA = 0.20 and beta_PA = 0.1.
bounds(
  events = c(60, 89, 110, 131, 178),
  power_int = 0.9, # beta_PA
  falsepos = 0.025, # gamma_FA
  hr_null = 1.3, # delta_null
  hr_alt = 0.8, # delta_alt
  rand_ratio = 1, # rand_ratio
  hr_marg_benefit = 0.95
)
#> $lhr_null
#> [1] 0.2623643
#> 
#> $lhr_alt
#> [1] -0.2231436
#> 
#> $lhr_pos
#> [1]  0.107751640  0.048544837  0.021238743  0.000795809 -0.031446759
#> 
#> $summary
#>   Deaths OS HR threshold for positivity One-sided false positive error rate
#> 1     60                          1.114                               0.275
#> 2     89                          1.050                               0.157
#> 3    110                          1.021                               0.103
#> 4    131                          1.001                               0.067
#> 5    178                          0.969                               0.025
#>   Level of 2-sided CI needed to rule out delta null
#> 1                                                45
#> 2                                                69
#> 3                                                79
#> 4                                                87
#> 5                                                95
#>   Probability of meeting positivity threshold under delta alt
#> 1                                                         0.9
#> 2                                                         0.9
#> 3                                                         0.9
#> 4                                                         0.9
#> 5                                                         0.9
#>   Posterior probability the true OS HR exceeds delta null given the data
#> 1                                                                  0.275
#> 2                                                                  0.157
#> 3                                                                  0.103
#> 4                                                                  0.067
#> 5                                                                  0.025
#>   Predictive probability the OS HR estimate at Final Analysis does not exceed the positivity threshold
#> 1                                                                                               25.394
#> 2                                                                                               29.681
#> 3                                                                                               32.744
#> 4                                                                                               35.977
#> 5                                                                                                   NA
#>   Probability of meeting positivity threshold under incremental benefit
#> 1                                                                 0.731
#> 2                                                                 0.681
#> 3                                                                 0.648
#> 4                                                                 0.617
#> 5                                                                 0.553
#> 
```
