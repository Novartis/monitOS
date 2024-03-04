
# monitOS: Monitoring overall survival in pivotal trials in indolent cancers

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/monitOS)](https://CRAN.R-project.org/package=monitOS)
<!-- badges: end -->

These guidelines are meant to provide a pragmatic, yet rigorous, help to drug developers and decision makers,
since they are shaped by three fundamental ingredients: the clinically determined margin of detriment on OS that
is unacceptably high (δnull); the benefit on OS that is plausible given the mechanism of action of the novel intervention (δalt);
and the quantity of information (i.e. events, expected number of survival events, at primary and final analysis) it is 
feasible to accrue given the clinical and drug development setting. The proposed guidelines facilitate
transparent discussions between stakeholders focusing on the risks of erroneous decisions and what might 
be an acceptable trade-off between power and the false positive error rate. 

Monitoring guidelines assume that the hazard ratio (HR) can adequately summarize the size of the benefits and harms of the experimental intervention vs control on overall survival (OS). Furthermore, guidelines assume that an OS HR < 1 is consistent with a beneficial effect of the intervention on OS (and smaller OS HRs <1 indicate increased efficacy). For more details about how OS monitoring guidelines are formulated, please refer to [arxiv paper](https://arxiv.org/).

## Installation

You can install the development version of monitOS like so:

``` r
# install.packages('monitOS')
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(monitOS)
# Example 01: OS monitoring guideline retrospectively applied to Motivating Example 1
# with delta null = 1.3, delta alt = 0.80, gamma_FA = 0.025 and  beta_PA = 0.10.
bounds(events=c(60, 89, 110, 131, 178),
       power_int=0.9,  # βPA
       falsepos=0.025,  # γFA
       hr_null = 1.3,  # δnull
       hr_alt = 0.8,   # δalt
       rand_ratio = 1,
       hr_marg_benefit = NULL)
# Example 02: OS monitoring guideline applied to Motivating Example 2
# with delta null = 4/3, delta alt = 0.7, gamma_FA = 0.20, beta_PA = 0.1, 
# randomization ratio 2 and 0.95 HR marginal benefit
bounds(events=c(60, 89, 110, 131, 178),
       power_int=0.9,  # βPA
       falsepos=0.025,  # γFA
       hr_null = 1.3,  # δnull
       hr_alt = 0.8,   # δalt
       rand_ratio = 2, # rand_ratio
       hr_marg_benefit = 0.95)  # Marginal HR benefit
```

