# rstanmulticore

A cross-platform (Windows, Linux, and Mac) R package to parallelize RStan MCMC 
chains across multiple cores. The syntax is very simple: replace calls to 
stan(...) with pstan(...).

## Examples
```r
library(rstan)
```

```
## Loading required package: Rcpp
## Loading required package: inline
## 
## Attaching package: 'inline'
## 
## The following object is masked from 'package:Rcpp':
## 
##     registerPlugin
## 
## rstan (Version 2.6.0, packaged: 2015-02-06 21:02:34 UTC, GitRev: 198082f07a60)
```

```r
## The data to analyze (Yes, it is very little!)
schools_dat <- list(
  J = 8, y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

## The Stan model for the data, stored as a string
schools_code <- 'data {
  int<lower=0> J; // number of schools 
  real y[J]; // estimated treatment effects
  real<lower=0> sigma[J]; // s.e. of effect estimates 
}
parameters {
  real mu; 
  real<lower=0> tau;
  real eta[J];
}
transformed parameters {
  real theta[J];
  for (j in 1:J)
    theta[j] <- mu + tau * eta[j];
}
model {
  eta ~ normal(0, 1);
  y ~ normal(theta, sigma);
}'
      
## Estimating the model 
fit.serial   <- stan( model_code = schools_code, data = schools_dat, 
                      iter = 1000, chains = 4, seed = 1)
```

```
## 
## TRANSLATING MODEL 'schools_code' FROM Stan CODE TO C++ CODE NOW.
## COMPILING THE C++ CODE FOR MODEL 'schools_code' NOW.
## ... snip ...
```


```r
library(rstanmulticore)
```

```
## Loading required package: parallel
```

```r
fit.parallel <- pstan( model_code = schools_code, data = schools_dat, 
                       iter = 1000, chains = 4, seed = 1)
```

```
## *** Parallel Stan run ***
## Working directory:
##  C:/Users/vanhoudnos-nathan/workspace/norc/spencer-5866.01.62/software/tmp
##  + Compiling the Stan model.
##  + Attempting  4  chains on 4 cores.
##    ... Creating the cluster.
##    ... Log file: stan-debug.2015-05-13-09.40.17.txt
##    ... Loading rstan on all workers.
##    ... Exporting the fitted model and data to all workers.
##    ... Running parallel chains.
##    ... Finished!
```


```r
all.equal( fit.serial@sim$samples, fit.parallel@sim$samples )
```

```
## [1] TRUE
```

You can also pass in a `fit` object: 

```r
fit.parallel.2 <- pstan( fit = fit.serial, data = schools_dat,
                         iter = 1000, chains = 4, seed = 1)
```

```
## *** Parallel Stan run ***
## Working directory:
##  C:/Users/vanhoudnos-nathan/workspace/norc/spencer-5866.01.62/software/tmp
##  + Compiled Stan model supplied.
##  + Attempting  4  chains on 4 cores.
##    ... Creating the cluster.
##    ... Log file: stan-debug.2015-05-13-09.40.51.txt
##    ... Loading rstan on all workers.
##    ... Exporting the fitted model and data to all workers.
##    ... Running parallel chains.
##    ... Finished!
```

```r
all.equal( fit.serial@sim$samples, fit.parallel.2@sim$samples )
```

```
## [1] TRUE
```

Or you use the `file` parameter from `stan`: 

```r
## First, what directory are we in? 
getwd()
```

```
## [1] "C:/Users/vanhoudnos-nathan/workspace/norc/spencer-5866.01.62/software/tmp"
```

```r
## Write out a txt file in our current directory containing the Stan code: 
write(schools_code, file="schools_code.txt")

## Use the txt file to run pstan
fit.parallel.3 <- pstan( file = 'schools_code.txt', data = schools_dat,
                         iter = 1000, chains = 4, seed = 1)
```

```
## *** Parallel Stan run ***
## Working directory:
##  C:/Users/vanhoudnos-nathan/workspace/norc/spencer-5866.01.62/software/tmp
##  + Compiling the Stan model.
##  + Attempting  4  chains on 4 cores.
##    ... Creating the cluster.
##    ... Log file: stan-debug.2015-05-13-09.40.57.txt
##    ... Loading rstan on all workers.
##    ... Exporting the fitted model and data to all workers.
##    ... Running parallel chains.
##    ... Finished!
```

```r
all.equal( fit.serial@sim$samples, fit.parallel.3@sim$samples )
```

```
## [1] TRUE
```
