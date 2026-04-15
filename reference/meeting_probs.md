# Probabilities of meeting positivity threshold under target HR

Probabilities of meeting positivity threshold under target HR

## Usage

``` r
meeting_probs(summary, lhr_pos, lhr_target = 1, rand_ratio = 1)
```

## Arguments

- summary:

  DataFrame. Summary dataframe from bounds.R

- lhr_pos:

  List. Log HRs for positive threshold

- lhr_target:

  Scalar. Target log HR to calculate the probability of meeting
  positivity thresholds

- rand_ratio:

  Integer. If patients are randomized k:1 between experimental
  intervention and control, rand_ratio should be inputted as k. Example:
  if patients are randomized 1:1 between experimental and control, k=1.
  If patients are randomized 2:1 between experimental and control, k=2.

## Value

Array. Probabilities of meeting positivity threshold under target HR
