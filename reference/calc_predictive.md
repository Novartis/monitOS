# Calculate posterior predictive probability of ruling out lhr_null at final OS analysis

Calculates the posterior predictive probability of 'ruling out' lhr_null
at final OS analysis given current estimate of OS log-HR is lhr_cont_k,
for k=1, ..., K-1

## Usage

``` r
calc_predictive(lhr_con, events)
```

## Arguments

- lhr_con:

  vector of length K (# number of looks at OS data) containing
  'continuation' thresholds on log-HR scale

- events:

  vector length K - number of OS events at each look at the data

## Value

vector of length K-1: continuation thresholds at analyses k=1, ..., K-1
expressed on scale of posterior predictive probability of ruling out
lhr_null at final OS analysis

## Examples

``` r
lhr_con <- c(0.2, 0.15, 0.1)
events <- c(100, 200, 300)
calc_predictive(lhr_con, events)
#> [1] 0.2701457 0.2701457
```
