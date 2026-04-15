# Find Positivity Threshold

This function calculates the positivity threshold based on various
criteria.

## Usage

``` r
find_pos(pos_thld, events, rand_ratio, hr_null, hr_alt, which_crit, targ)
```

## Arguments

- pos_thld:

  Numeric. The initial positivity threshold.

- events:

  Numeric vector of length 2. The number of events at each analysis.

- rand_ratio:

  Numeric. The randomization ratio.

- hr_null:

  Numeric. The hazard ratio under the null hypothesis.

- hr_alt:

  Numeric. The hazard ratio under the alternative hypothesis.

- which_crit:

  Integer. The criterion to be used for finding the positivity
  threshold:

  - 1: False positive / False negative equals required value.

  - 2: False positive / (False negative + False positive) equals
    required value.

  - 3: False positive equals required value.

  - 4: Predictive probability equals required value.

- targ:

  Numeric. The target value for the chosen criterion.

## Value

Numeric. The calculated positivity threshold based on the specified
criterion.

## Examples

``` r
find_pos(
  pos_thld = 1.5,
  events = c(100, 200),
  rand_ratio = 1,
  hr_null = 1,
  hr_alt = 1.5,
  which_crit = 1,
  targ = 0.05
)
#> [1] 1.907371
```
