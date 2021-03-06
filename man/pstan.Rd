% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/pstan.R
\name{pstan}
\alias{pstan}
\title{Fit a model using Stan on multiple cores}
\usage{
pstan(file, model_name = "anon_model", model_code = "", fit = NA,
  data = list(), pars = NA, chains = 4, iter = 2000,
  warmup = floor(iter/2), thin = 1, init = "random", seed = NULL,
  algorithm = c("NUTS", "HMC", "Fixed_param"), control = NULL, sample_file,
  diagnostic_file, save_dso = TRUE, verbose = FALSE, ...,
  boost_lib = NULL, eigen_lib = NULL, pdebug = TRUE)
}
\arguments{
\item{file}{A character string file name or a connection that R supports containing the text of a model specification in the Stan modeling language; a model may also be specified directly as a character string using parameter \code{model_code} or through a previous fit using parameter \code{fit}. When \code{fit} is specified, parameter \code{file} is ignored.}

\item{model_name}{A character string naming the model; defaults to \code{"anon_model"}. However, the model name would be derived from \code{file} or \code{model_code} (if \code{model_code} is the name of a character string object) if \code{model_name} is not specified.}

\item{model_code}{A character string either containing the model definition or the name of a character string object in the workspace. This parameter is used only if parameter \code{file} is not specified. When \code{fit} is specified, the model compiled previously is used so specifying \code{model_code} is ignored.}

\item{fit}{An instance of S4 class \code{stanfit} derived from a previous fit; defaults to \code{NA}. If fit is not \code{NA}, the compiled model associated with the fitted result is re-used; thus the time that would otherwise be spent recompiling the C++ code for the model can be saved.}

\item{data}{An object of class \code{list, environment} providing the data for the model, or a vector of character strings for all the names of objects used as data in the working space. See \code{help(stan)} for further details.}

\item{chains}{A positive integer specifying number of chains; defaults to 4. When possible, each chain is executed on its own core.}

\item{seed}{The seed, a positive integer, for random number generation of Stan. The default is generated from 1 to the maximum integer supported by R so fixing the seed of R's random number generator can essentially fix the seed of Stan. When multiple chains are used, only one seed is needed, with other chains' seeds being generated from the first chain's seed to prevent dependency among the random number streams for the chains. When a seed is specified by a number, as.integer will be applied to it. If as.integer produces NA, the seed is generated randomly. We can also specify a seed using a character string of digits, such as "12345", which is converted to integer.}

\item{...}{The additional arguments are passed directly to \code{rstan::stan}. See \code{help(stan)} for documentation.}

\item{pdebug}{If \code{TRUE}, \code{pstan} will create a file in the current working directory,
\code{stan-debug-*}, that contains the output of the \code{rstan::stan} calls. Defaults to \code{TRUE}.}
}
\value{
Fitted results as an object of S4 class \code{stanfit}. If error occurs before or during sampling, and if \code{test_grad = TRUE}, the returned object would not contain samples. But the compiled binary object for the model is still included, so we can reuse the returned object for another sampling.
}
\description{
A wrapper for \code{rstan::stan} that, when possible, runs each MCMC chain on a
separate core. See \code{help(stan)} for details of the syntax. In the documentation below,
the stan help page is quoted without further attribution.
}
\details{
\code{pstan} does all of the work of fitting a Stan model and returning the results as
an instance of \code{stanfit}. First, it translates the Stan model to C++ code.
Second, the C++ code is compiled into a binary shared object, which is loaded into
the current R session (an object of S4 class \code{stanmodel} is created).
Third, \code{pstan} calls \code{parallel::makeCluster} to setup a parallel worker
process for every chain (or the maximum number of cores, whichever is less), distributes
the compiled \code{stanmodel} to each worker, and uses
\code{parallel:parLapply} to execute \code{stan:rstan} on each core.
Finally, the chains are collected and wrapped into an object of S4 class \code{stanfit},
which provides functions such as print, summary, and plot to inspect and retrieve the
results of the fitted model.

Note that \code{pstan} can also be used to sample again from a fitted model under
different settings (e.g., different \code{iter}) by providing argument fit. In this case,
the compiled C++ code for the model is reused.
}

