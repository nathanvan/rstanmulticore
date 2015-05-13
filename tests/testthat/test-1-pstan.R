## Tests for pstan.R

## Weird errors unless I add the following:
Sys.setenv("R_TESTS" = "")
## from https://github.com/hadley/testthat/issues/144

context('pstan.R tests')

context(' + Checking error handling')
  test_that("bad Stan code",{

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
    
    
    ## There is no such type as realBUG. 
    schools_code_buggy <- 'data {
      int<lower=0> J; // number of schools 
      real y[J]; // estimated treatment effects
      real<lower=0> sigma[J]; // s.e. of effect estimates 
    }
      parameters {
      realBUG mu; 
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
        
      schools_dat <- list(J = 8, 
                          y = c(28,  8, -3,  7, -1,  1, 18, 12),
                          sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
      schools_dat_buggy <- list(J = 8, 
                        y = c(28,  8, -3,  7, -1,  1, 18, 12),
                        sigmaBug = c(15, 10, 16, 11,  9, 11, 10, 18))    
      expect_that( 
        pstan(model_code = schools_code_buggy, data=schools_dat,
                     iter = 1000, chains = 4, seed=1, pdebug=FALSE),
        throws_error())

      valid.stanfit <- pstan( model_code = schools_code, data=schools_dat, 
                                chains=0, pdebug=FALSE)
      suppressMessages( 
        broken.fit <- pstan(fit = valid.stanfit, data = schools_dat_buggy, 
                          iter = 1000, chains = 4, seed=1, pdebug=FALSE) )

      expect_that( 
        all.equal(broken.fit$fit, valid.stanfit), is_true() )
      expect_that(
        length( broken.fit$parallel.list ), equals(4) )
            
      })


context(' + Checking equivalence of serial and parallel runs')
    test_that("pstan",{
        
      # From https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
      
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
      
      schools_dat <- list(J = 8, 
                          y = c(28,  8, -3,  7, -1,  1, 18, 12),
                          sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
      
      ## Check if pstan and stan end up with the same random seed 
      ## given R's current state
      set.seed(1)
      beQuiet <- capture.output( 
        fit   <- stan(model_code = schools_code, data = schools_dat, 
                      iter = 1000, chains = 4) )
      set.seed(1)
      fit.p.1 <- pstan(model_code = schools_code, data = schools_dat, 
                      iter = 1000, chains = 4, pdebug=FALSE)      
      
      ## Are the stan arguments the same? (Including seed.)
      expect_that( all.equal(fit@stan_args, fit.p.1@stan_args), is_true() )
      
      ## Are the parameters with the same seed the same? 
      expect_that(
        extract( fit, pars='mu', permuted=FALSE, inc_warmup=TRUE  ),
        is_identical_to( extract(fit.p.1, pars='mu', permuted=FALSE, inc_warmup=TRUE)))
      expect_that(
        extract( fit, pars='tau', permuted=FALSE, inc_warmup=TRUE  ),
        is_identical_to( extract(fit.p.1, pars='tau', permuted=FALSE, inc_warmup=TRUE)))
      expect_that(
        extract( fit, pars='theta', permuted=FALSE, inc_warmup=TRUE  ),
        is_identical_to( extract(fit.p.1, pars='theta', permuted=FALSE, inc_warmup=TRUE)))

      ## Check that a different seed is generated even if the a fit object
      ## is passed in 
      fit.p.2 <- pstan(fit = fit, data = schools_dat, 
                       iter = 1000, chains = 4, pdebug=FALSE)
      expect_that( fit@stan_args[[1]]$seed == fit.p.2@stan_args[[1]]$seed, 
                   is_false()) 
      
      ## Are parameters with a different seed different? 
      expect_that(
        ( all.equal( 
            extract( fit, pars='mu', permuted=FALSE, inc_warmup=TRUE  ),
            extract( fit.p.2, pars='mu', permuted=FALSE, inc_warmup=TRUE )) == TRUE),
        is_false()) 
      expect_that(
        ( all.equal( 
          extract( fit, pars='tau', permuted=FALSE, inc_warmup=TRUE  ),
          extract( fit.p.2, pars='tau', permuted=FALSE, inc_warmup=TRUE )) == TRUE),
        is_false()) 
      expect_that(
        ( all.equal( 
          extract( fit, pars='theta', permuted=FALSE, inc_warmup=TRUE  ),
          extract( fit.p.2, pars='theta', permuted=FALSE, inc_warmup=TRUE )) == TRUE),
        is_false())   
            
      
      ## Check that stan an pstan give same answer with chains = 1
      time.obj <- system.time( beQuiet <- capture.output( 
        fit.c1   <- stan(model_code = schools_code, data = schools_dat, 
                      iter = 1000, chains = 1, seed = 3) ) )
      
      time.obj.par <- system.time({
        fit.p.c1 <- pstan(model_code = schools_code, data = schools_dat, 
                       iter = 1000, chains = 1, seed = 3, pdebug=FALSE)
      })
      
      expect_that( all.equal( fit.c1@sim$samples, 
                              fit.p.c1@sim$samples ), is_true() )
      
      ## I don't feel like there is much I can do with the time info. 
      ## 
      ## time.obj['user.self']
      ## time.obj.par['user.self']
      
      ## Check that pstan accepts valid stan arguments that are not part of 
      ## its function signature when they are passed as variables
      iter.var <- 1000
      fit.p.c1.v <- pstan(fit=fit.p.c1, data = schools_dat, 
                        iter = iter.var, chains = 1, seed = 3, pdebug=FALSE)
      expect_that( all.equal( fit.p.c1@sim$samples, 
                              fit.p.c1.v@sim$samples ), is_true() )
    })
