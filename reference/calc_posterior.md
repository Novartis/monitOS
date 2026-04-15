# Function which calculates for k=1, ..., K, Pr(log-HR \>= lhr_null \| theta.hat.k = lhr_con.k)

i.e. the posterior probability the true OS log-hr exceeds the minimum
unacceptable OS log-HR given the estimate of the log-hr at analysis k
equals lhr_con.k (i.e. the estimate is equal to the stage k
'continuation threshold').

## Usage

``` r
calc_posterior(lhr_con, lhr_null, events)
```

## Arguments

- lhr_con:

  vector of length K (# number of looks at OS data) containing
  'continuation' thresholds on log-HR scale

- lhr_null:

  scalar - minimum unacceptable OS log-HR

- events:

  vector length K - number of OS events at each look at the data

## Value

vector of length K - continuation thresholds expressed on posterior
probability scale

## Examples

``` r
lhr_con <- c(0.2, 0.15, 0.1)
lhr_null <- 0.25
events <- c(100, 200, 300)
calc_posterior(lhr_con, lhr_null, events)
#> [1] 0.40129367 0.23975006 0.09696543
```
