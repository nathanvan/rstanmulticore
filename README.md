# rstanmulticore

A cross-platform (Windows, Linux, and Mac) R package to parallelize RStan MCMC 
chains across multiple cores. The syntax is very simple: replace calls to 
stan(...) with pstan(...).

## Installation 
Step 0.A : If you do not already have rstan installed, install it using the instructions [here](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).

Step 0.B: If you do not already have devtools installed, install it using the instructions [here](http://www.rstudio.com/products/rpackages/devtools/).

Step 1: Install rstanmulticore directly from my GitHub repository using install_github('nathanvan/rstanmulticore').

```r
> library(devtools)
> install_github('nathanvan/rstanmulticore')
Downloading github repo nathanvan/rstanmulticore@master
Installing rstanmulticore
  ... snip ... 
* DONE (rstanmulticore)
```

## Usage examples

We begin with the default "Eight Schools" example from the [Quick Start Guide](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#example-1-eight-schools) using the default `stan` function:

```r
library(rstan)
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

## The data to analyze (Yes, it is very little!)
schools_dat <- list(
  J = 8, y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

## The Stan model for the data, stored as a string
schools_code <- 'data {
  int J; // number of schools 
  real y[J]; // estimated treatment effects
  real sigma[J]; // s.e. of effect estimates 
}
parameters {
  real mu; 
  real tau;
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
      
## The data to analyze (Yes, it is very little!)
schools_dat <- list(
  J = 8, y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

## The Stan model for the data, stored as a string
schools_code <- 'data {
  int J; // number of schools 
  real y[J]; // estimated treatment effects
  real sigma[J]; // s.e. of effect estimates 
}
parameters {
  real mu; 
  real tau;
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
## 
## TRANSLATING MODEL 'schools_code' FROM Stan CODE TO C++ CODE NOW.
## COMPILING THE C++ CODE FOR MODEL 'schools_code' NOW.
## cygwin warning:
##   MS-DOS style path detected: C:/PROGRA~1/R/R-31~1.3/etc/x64/Makeconf
##   Preferred POSIX equivalent is: /cygdrive/c/PROGRA~1/R/R-31~1.3/etc/x64/Makeconf
##   CYGWIN environment variable option "nodosfilewarning" turns off this warning.
##   Consult the user's guide for more details about POSIX paths:
##     http://cygwin.com/cygwin-ug-net/using.html#using-pathnames
## 
## SAMPLING FOR MODEL 'schools_code' NOW (CHAIN 1).
##
##  ... < snip > ...  
## 
## SAMPLING FOR MODEL 'schools_code' NOW (CHAIN 2).
## 
##  ... < snip > ... 
## 
## SAMPLING FOR MODEL 'schools_code' NOW (CHAIN 3).
## 
##  ... < snip > ... 
## 
## SAMPLING FOR MODEL 'schools_code' NOW (CHAIN 4).
## 
##  ... < snip > ... 
```

Note that `stan` is pretty verbose.

I chose to make `pstan` less verbose. By default, `pstan` reports sparse progress information to the R console and the more detailed information is redirected to a file, `stan-debug-*`, that is created in the current working directory. (If you wish to see the detailed info in real time, use `tail -f` in your shell.)

Usage of `pstan` as follows:

```r
library(rstanmulticore)
## Loading required package: parallel

fit.parallel <- pstan( model_code = schools_code, data = schools_dat, 
                       iter = 1000, chains = 4, seed = 1)
## *** Parallel Stan run ***
## Working directory:
##  C:/Users/vanhoudnos-nathan/workspace/norc/spencer-5866.01.62/software/tmp
##  + Compiling the Stan model.
##  + Attempting  4  chains on 4 cores.
##    ... Creating the cluster.
##    ... Log file: stan-debug.2015-05-01-12.38.21.txt
##    ... Loading rstan on all workers.
##    ... Exporting the fitted model and data to all workers.
##    ... Running parallel chains.
##    ... Finished!
```

It is also possible to pass in a previously fit model object with `fit`: 
```r
fit.parallel.2 <- pstan( fit = fit.serial, data = schools_dat,
                         iter = 1000, chains = 4, seed = 1)
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

Or use the `file` parameter from `stan`: 
```r
## Write out a txt file in our current directory containing the Stan code: 
write(schools_code, file="schools_code.txt")

## Use the txt file to run pstan
fit.parallel.3 <- pstan( file = 'schools_code.txt', data = schools_dat,
                         iter = 1000, chains = 4, seed = 1)
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

As promised, the output -- the actual samples drawn from the posterior -- of `pstan` is identical to that of `stan` in all cases:

```r
all.equal( fit.serial@sim$samples, fit.parallel@sim$samples )
## [1] TRUE

all.equal( fit.serial@sim$samples, fit.parallel.2@sim$samples )
## [1] TRUE

all.equal( fit.serial@sim$samples, fit.parallel.3@sim$samples )
## [1] TRUE
```