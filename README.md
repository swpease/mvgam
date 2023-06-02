
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *mvgam*

The goal of `mvgam` is to use a Bayesian framework to estimate
parameters of Generalized Additive Models (DGAMs) for time series with
dynamic trend components. The motivation for the package and some of its
primary objectives are described in detail by [Clark & Wells
2022](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13974)
(published in *Methods in Ecology and Evolution*), with additional
inspiration on the use of Bayesian probabilistic modelling to quantify
uncertainty and advise principled decision making coming from [Michael
Betancourt](https://betanalpha.github.io/writing/), [Michael
Dietze](https://www.bu.edu/earth/profiles/michael-dietze/) and [Emily
Fox](https://emilybfox.su.domains/), among many others.

## Disclaimer

Please note that the `mvgam` package is under active development, and it
is likely that certain functionality will change on a regular basis. As
such, please take caution when implementing the package for automated
routines. Also note that, because of the many advantages of `Stan` over
`JAGS`, *further development of the package will only be applied to
`Stan`*. This includes the planned addition of more response
distributions, plans to handle zero-inflation, and plans to incorporate
a greater variety of trend models. Users are strongly encouraged to opt
for `Stan` over `JAGS` in any proceeding workflows.

## Resources

A number of case studies have been compiled to highlight how DGAMs can
be estimated using MCMC sampling. These are hosted currently on `RPubs`
at the following links:

- [mvgam case study 1: model comparison and data
  assimilation](https://rpubs.com/NickClark47/mvgam)
- [mvgam case study 2: multivariate
  models](https://rpubs.com/NickClark47/mvgam2)
- [mvgam case study 3: distributed lag
  models](https://rpubs.com/NickClark47/mvgam3)

The package can also be used to generate all necessary data structures,
initial value functions and modelling code necessary to fit DGAMs using
`Stan` or `JAGS`. This can be helpful if users wish to make changes to
the model to better suit their own bespoke research / analysis goals.
The following resources can be helpful to troubleshoot:

- [Stan Discourse](https://discourse.mc-stan.org/)
- [JAGS Discourse](https://sourceforge.net/projects/mcmc-jags/)

## Installation

Install the development version from `GitHub` using:
`devtools::install_github("nicholasjclark/mvgam")`. Note that to
actually condition models with MCMC sampling, either the `JAGS` software
must be installed (along with the `R` packages `rjags` and `runjags`) or
the `Stan` software must be installed (along with either `rstan` and/or
`cmdstanr`). Only `rstan` is listed as a dependency of `mvgam` to ensure
that installation is less difficult. If users wish to fit the models
using `mvgam`, please refer to installation links for `JAGS`
[here](https://sourceforge.net/projects/mcmc-jags/files/), for `Stan`
with `rstan` [here](https://mc-stan.org/users/interfaces/rstan), or for
`Stan` with `cmdstandr` [here](https://mc-stan.org/cmdstanr/). You will
need a fairly recent version of `Stan` to ensure all the model syntax is
recognized. If you see warnings such as
`variable "array" does not exist`, this is usually a sign that you need
to update your version of `Stan`. We highly recommend you use `Cmdstan`
through the `cmdstanr` interface as the backend. This is because
`Cmdstan` is easier to install, is more up to date with new features,
and uses less memory than `Rstan`. See [this documentation from the
`Cmdstan` team for more
information](http://mc-stan.org/cmdstanr/articles/cmdstanr.html#comparison-with-rstan).

## Citing mvgam and related software

When using open source software (or software in general), please make
sure to appropriately acknowledge the hard work that developers and
maintainers put into making these packages available. Citations are
currently the best way to formally acknowledge this work, so we highly
encourage you to cite any packages that you rely on for your research.

When using `mvgam`, please cite the following publication:

- Clark, N.J. and Wells, K. (2022). Dynamic Generalized Additive Models
  (DGAMs) for forecasting discrete ecological time series. *Methods in
  Ecology and Evolution*. DOI: <https://doi.org/10.1111/2041-210X.13974>

As `mvgam` acts as an interface to `Stan` and `JAGS`, please
additionally cite whichever software you use for parameter estimation:

- Carpenter B., Gelman A., Hoffman M. D., Lee D., Goodrich B.,
  Betancourt M., Brubaker M., Guo J., Li P., and Riddell A. (2017).
  Stan: A probabilistic programming language. *Journal of Statistical
  Software*. 76(1). 10.18637/jss.v076.i01
- Plummer, M. (2013). JAGS: A program for analysis of Bayesian graphical
  models using Gibbs sampling. *Proceedings of the 3rd International
  Workshop on Distributed Statistical Computing*. 124(125.10).

`mvgam` relies on several other `R` packages and, of course, on `R`
itself. To find out how to cite R and its packages, use the `citation`
function. There are some features of `mvgam` which specifically rely on
certain packages. The most important of these is the generation of data
necessary to estimate smoothing splines, which entirely rely on `mgcv`.
The `rstan` and `cmdstanr` packages together with `Rcpp` makes `Stan`
conveniently accessible in `R`, while the `rjags` and `runjags` packages
together with the `coda` package make `JAGS` accessible in `R`. If you
use some of these features, please also consider citing the related
packages.

## Dynamic latent temporal processes

`mvgam` is designed to propagate unobserved temporal processes to
capture autocorrelation in the observed time series. This works in a
state-space format, with the temporal *trend* evolving independently of
the observation process. Available trend models are:

- `RW` Random Walk
- `AR1` Autoregressive model with AR coefficient for lag 1
- `AR2` Autoregressive model with AR coefficients for lags 1 and 2
- `AR3` Autoregressive model with AR coefficients for lags 1, 2 and 3
- `VAR1` Vector Autoregressive model with VAR coefficients for lag 1;
  contemporaneously uncorrelated errors
- `GP` Squared exponential Gaussian Process
- `None` No latent trend is fitted

When using `Stan` as the back-end, all of these trend models (apart from
`VAR1`) can be estimated using a set of dimension-reduced dynamic
factors. Please see [mvgam case study 2: multivariate
models](https://rpubs.com/NickClark47/mvgam2) for more information

## A brief introduction to the package

We can explore the model’s primary functions using a test dataset that
is available with all `R` installations. We introduce Dynamic
Generalized Additive Models and some of the key utility functions
provided in `mvgam`. First, load the `lynx` data and plot the series as
well as its estimated autocorrelation function

``` r
library(mvgam)
#> Loading required package: mgcv
#> Warning: package 'mgcv' was built under R version 4.2.2
#> Loading required package: nlme
#> This is mgcv 1.8-41. For overview type 'help("mgcv-package")'.
#> Loading required package: parallel
#> Loading required package: rstan
#> Loading required package: StanHeaders
#> 
#> rstan version 2.26.13 (Stan version 2.26.1)
#> For execution on a local, multicore CPU with excess RAM we recommend calling
#> options(mc.cores = parallel::detectCores()).
#> To avoid recompilation of unchanged Stan programs, we recommend calling
#> rstan_options(auto_write = TRUE)
#> For within-chain threading using `reduce_sum()` or `map_rect()` Stan functions,
#> change `threads_per_chain` option:
#> rstan_options(threads_per_chain = 1)
#> Do not specify '-march=native' in 'LOCAL_CPPFLAGS' or a Makevars file
#> Welcome to mvgam. Please cite as: Clark, NJ, and Wells, K. 2022. Dynamic Generalized Additive Models (DGAMs) for forecasting discrete ecological time series. Methods in Ecology and Evolution, 2022, https://doi.org/10.1111/2041-210X.13974
data(lynx)
lynx_full = data.frame(year = 1821:1934, 
                       population = as.numeric(lynx))
plot(lynx_full$population, type = 'l', ylab = 'Lynx trappings',
     xlab = 'Time', bty = 'l', lwd = 2)
box(bty = 'l', lwd  = 2)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="60%" style="display: block; margin: auto;" />

``` r
acf(lynx_full$population, main = '', bty = 'l', lwd = 2,
    ci.col = 'darkred')
box(bty = 'l', lwd  = 2)
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="60%" style="display: block; margin: auto;" />

Along with serial autocorrelation, there is a clear \~19-year cyclic
pattern to the data. Create a `season` term that can be used to model
this effect and give a better representation of the data generating
process than we would likely get with a linear model

``` r
plot(stl(ts(lynx_full$population, frequency = 19), s.window = 'periodic'),
     lwd = 2, col.range = 'darkred')
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="60%" style="display: block; margin: auto;" />

``` r
lynx_full$season <- (lynx_full$year %%19) + 1
```

For `mvgam` models, we need an indicator of the series name as a
`factor` variable (if the column `series` is missing, this will be added
automatically by assuming that all observations are from a single time
series). Finally, a `time` column is needed to index time

``` r
lynx_full$time <- 1:NROW(lynx_full)
lynx_full$series <- factor('series1')
```

Split the data into training (first 50 years) and testing (next 10 years
of data) to evaluate multi-step ahead forecasts

``` r
lynx_train = lynx_full[1:50, ]
lynx_test = lynx_full[51:60, ]
```

Inspect the series in a bit more detail using `mvgam`’s plotting utility

``` r
plot_mvgam_series(data = lynx_train, y = 'population')
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="60%" style="display: block; margin: auto;" />

Now we will formulate an `mvgam` model; this model fits a GAM in which a
cyclic smooth function for `season` is estimated jointly with a full
time series model for the temporal process (in this case an `AR3`
process), rather than relying on smoothing splines that do not
incorporate a concept of the future. We assume the outcome follows a
Poisson distribution. But before conditioning the model on observed
data, a check of prior smooth function realisations is useful to ensure
we are allowing enough flexibility to capture the types of functional
behaviours we think are reasonable without allowing outrageous
behaviours. First we follow conventional recommendations to set `k` for
the smooth term to be large, which would allow maximum flexibility in
functional behaviours

``` r
lynx_mvgam_prior <- mvgam(data = lynx_train,
               formula = population ~ s(season, bs = 'cc', k = 19),
               knots = list(season = c(0.5, 19.5)),
               family = 'poisson',
               trend_model = 'AR3',
               chains = 4,
               prior_simulation = TRUE)
```

Plot empirical quantiles of the prior seasonal smooth function

``` r
plot(lynx_mvgam_prior, type = 'smooths')
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="60%" style="display: block; margin: auto;" />

Plot a set of realisations from the prior seasonal smooth function

``` r
plot(lynx_mvgam_prior, type = 'smooths', realisations = TRUE,
     n_realisations = 20)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="60%" style="display: block; margin: auto;" />

These functions are showing the marginal contribution of the seasonal
smooth function to the linear predictor (on the log scale), and they are
clearly allowed to move into ridiculous spaces that we should give very
little prior plausibility to:

``` r
exp(-15)
#> [1] 3.059023e-07
exp(15)
#> [1] 3269017
```

Setting `k` to a smaller value results in less flexibility. This is
because number of basis functions that contribute to functional
behaviour is reduced

``` r
lynx_mvgam_prior <- mvgam(data = lynx_train,
               formula = population ~ s(season, bs = 'cc', k = 12),
               knots = list(season = c(0.5, 19.5)),
               family = 'poisson',
               trend_model = 'AR3',
               chains = 4,
               prior_simulation = TRUE)
```

The resulting prior looks more reasonable given the range of the
observations, and there is clearly enough flexibility to support a wide
range of functional shapes.

``` r
plot(lynx_mvgam_prior, type = 'smooths')
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="60%" style="display: block; margin: auto;" />

``` r
plot(lynx_mvgam_prior, type = 'smooths', realisations = TRUE,
     n_realisations = 20)
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="60%" style="display: block; margin: auto;" />

In practice, imparting domain knowledge into prior specifications for
penalised smooth functions is challenging, as these behaviours are often
the cumulative result of multiple penalty matrices that all have their
own separate smoothing parameters. Changing the prior on the smoothing
parameters is another option (`mvgam` uses a half-normal prior by
default, which regularises functions more than the default approach used
in `mgcv::jagam`). But without running through prior visualisations (and
other prior pushforward checks) it will be more difficult to reason
about how to set `k` to respect domain knowledge. In general it is
highly recommended that users view `mvgam` and related interfaces such
as `brms` as tools for building scaffold models that can then be
modified to suit the bespoke needs of each particular analysis.

Users can also check what the default prior distributions are for given
model formulations, which can be helpful to understand how the model can
be modified but also to see any restrictions on what can be changed
within the `mvgam` framework.

``` r
test_priors <- get_mvgam_priors(population ~ s(season, bs = 'cc', k = 12),
                                family = 'poisson',
                                data = lynx_train,
                                trend_model = 'AR3',
                                use_stan = TRUE)
test_priors
#>                                    param_name param_length
#> 1               vector<lower=0>[n_sp] lambda;            1
#> 2 vector<lower=-1.5,upper=1.5>[n_series] ar1;            1
#> 3 vector<lower=-1.5,upper=1.5>[n_series] ar2;            1
#> 4 vector<lower=-1.5,upper=1.5>[n_series] ar3;            1
#> 5            vector<lower=0>[n_series] sigma;            1
#>                    param_info                    prior
#> 1 s(season) smooth parameters lambda ~ normal(10, 25);
#> 2       trend AR1 coefficient      ar1 ~ std_normal();
#> 3       trend AR2 coefficient      ar2 ~ std_normal();
#> 4       trend AR3 coefficient      ar3 ~ std_normal();
#> 5                    trend sd  sigma ~ exponential(2);
#>                example_change new_lowerbound new_upperbound
#> 1 lambda ~ exponential(0.93);             NA             NA
#> 2   ar1 ~ normal(0.24, 0.57);             NA             NA
#> 3   ar2 ~ normal(0.98, 0.16);             NA             NA
#> 4  ar3 ~ normal(-0.23, 0.41);             NA             NA
#> 5  sigma ~ exponential(0.09);             NA             NA
```

Any of the above priors can be changed by modifying the `prior` column
and supplying the resulting `data.frame` to the `priors` argument in
`mvgam()`. But for now, we will proceed with the defaults by
conditioning the model on observed data in `Stan` using MCMC sampling
with the `Cmdstan` interface (installation links for `rstan` and
`cmdstanr` are found [here](https://mc-stan.org/users/interfaces/rstan)
and [here](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)).

``` r
lynx_mvgam <- mvgam(data = lynx_train,
               newdata = lynx_test,
               formula = population ~ s(season, bs = 'cc', k = 12),
               knots = list(season = c(0.5, 19.5)),
               family = 'poisson',
               trend_model = 'AR3',
               use_stan = TRUE)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 1 finished in 23.1 seconds.
#> Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 2 finished in 23.2 seconds.
#> Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 4 finished in 24.1 seconds.
#> Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 3 finished in 25.7 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 24.0 seconds.
#> Total execution time: 25.8 seconds.
```

Inspect the resulting model file, which is written in the `Stan`
probabilistic programming language

``` r
code(lynx_mvgam)
#> // Stan model code generated by package mvgam
#> data {
#>   int<lower=0> total_obs; // total number of observations
#>   int<lower=0> n; // number of timepoints per series
#>   int<lower=0> n_sp; // number of smoothing parameters
#>   int<lower=0> n_series; // number of series
#>   int<lower=0> num_basis; // total number of basis coefficients
#>   vector[num_basis] zero; // prior locations for basis coefficients
#>   real p_taus[1]; // prior precisions for parametric coefficients
#>   real p_coefs[1]; // prior locations for parametric coefficients
#>   matrix[total_obs, num_basis] X; // mgcv GAM design matrix
#>   int<lower=0> ytimes[n, n_series]; // time-ordered matrix (which col in X belongs to each [time, series] observation?)
#>   matrix[10,10] S1; // mgcv smooth penalty matrix S1
#>   int<lower=0> n_nonmissing; // number of nonmissing observations
#>   int<lower=0> flat_ys[n_nonmissing]; // flattened nonmissing observations
#>   matrix[n_nonmissing, num_basis] flat_xs; // X values for nonmissing observations
#>   int<lower=0> obs_ind[n_nonmissing]; // indices of nonmissing observations
#> }
#> parameters {
#>   // raw basis coefficients
#>   vector[num_basis] b_raw;
#>   // latent trend AR1 terms
#>   vector<lower=-1.5,upper=1.5>[n_series] ar1;
#>   // latent trend AR2 terms
#>   vector<lower=-1.5,upper=1.5>[n_series] ar2;
#>   // latent trend AR3 terms
#>   vector<lower=-1.5,upper=1.5>[n_series] ar3;
#>   // latent trend variance parameters
#>   vector<lower=0>[n_series] sigma;
#>   // latent trends
#>   matrix[n, n_series] trend;
#>   // smoothing parameters
#>   vector<lower=0>[n_sp] lambda;
#> }
#> transformed parameters {
#>   // basis coefficients
#>   vector[num_basis] b;
#>   b[1:num_basis] = b_raw[1:num_basis];
#> }
#> model {
#>   // parametric effect priors (regularised for identifiability)
#>   for (i in 1:1) {
#>   b_raw[i] ~ normal(p_coefs[i], sqrt(1 / p_taus[i]));
#>   }
#>   // prior for s(season)...
#>   b_raw[2:11] ~ multi_normal_prec(zero[2:11],S1[1:10,1:10] * lambda[1]);
#>   // priors for AR parameters
#>   ar1 ~ std_normal();
#>   ar2 ~ std_normal();
#>   ar3 ~ std_normal();
#>   // priors for smoothing parameters
#>   lambda ~ normal(10, 25);
#>   // priors for latent trend variance parameters
#>   sigma ~ exponential(2);
#>   // trend estimates
#>   trend[1, 1:n_series] ~ normal(0, sigma);
#>   trend[2, 1:n_series] ~ normal(trend[1, 1:n_series] * ar1, sigma);
#>   trend[3, 1:n_series] ~ normal(trend[2, 1:n_series] * ar1 + trend[1, 1:n_series] * ar2, sigma);
#>   for(s in 1:n_series){
#>   trend[4:n, s] ~ normal(ar1[s] * trend[3:(n - 1), s] + ar2[s] * trend[2:(n - 2), s] + ar3[s] * trend[1:(n - 3), s], sigma[s]);
#>   }
#>   {
#>   // likelihood functions
#>   vector[n_nonmissing] flat_trends;
#>   flat_trends = (to_vector(trend))[obs_ind];
#>   flat_ys ~ poisson_log_glm(append_col(flat_xs, flat_trends),
#>   0.0,append_row(b, 1.0));
#>   }
#> }
#> generated quantities {
#>   vector[total_obs] eta;
#>   matrix[n, n_series] mus;
#>   vector[n_sp] rho;
#>   vector[n_series] tau;
#>   array[n, n_series] int ypred;
#>   rho = log(lambda);
#>   for (s in 1:n_series) {
#>   tau[s] = pow(sigma[s], -2.0);
#>   }
#>   // posterior predictions
#>   eta = X * b;
#>   for(s in 1:n_series){ 
#>   mus[1:n, s] = eta[ytimes[1:n, s]] + trend[1:n, s];
#>   ypred[1:n, s] = poisson_log_rng(mus[1:n, s]);
#>   }
#> }
```

Perform a series of posterior retrodictive checks to see if the model is
able to simulate data for the training period that looks realistic and
unbiased. First, examine histograms for posterior retrodictions (`yhat`)
and compare to the histogram of the observations (`y`)

``` r
ppc(lynx_mvgam, series = 1, type = 'hist')
```

<img src="man/figures/README-unnamed-chunk-19-1.png" width="60%" style="display: block; margin: auto;" />

Now plot the distribution of predicted means compared to the observed
mean

``` r
ppc(lynx_mvgam, series = 1, type = 'mean')
```

<img src="man/figures/README-unnamed-chunk-20-1.png" width="60%" style="display: block; margin: auto;" />

Next examine simulated empirical Cumulative Distribution Functions (CDF)
for posterior retrodictions (`yhat`) and compare to the CDF of the
observations (`y`)

``` r
ppc(lynx_mvgam, series = 1, type = 'cdf')
```

<img src="man/figures/README-unnamed-chunk-21-1.png" width="60%" style="display: block; margin: auto;" />

Rootograms are becoming [popular graphical tools for checking a discrete
model’s ability to capture dispersion properties of the response
variable](https://arxiv.org/pdf/1605.01311.pdf). Posterior predictive
hanging rootograms can be displayed using the `ppc()` function in
`mvgam`. In the plot below, we bin the unique observed values into `25`
bins to prevent overplotting and help with interpretation. This plot
compares the frequencies of observed vs predicted values for each bin,
which can help to identify aspects of poor model fit. For example, if
the gray bars (representing observed frequencies) tend to stretch below
zero, this suggests the model’s simulations predict the values in that
particular bin less frequently than they are observed in the data. A
well-fitting model that can generate realistic simulated data will
provide a rootogram in which the lower boundaries of the grey bars are
generally near zero

``` r
ppc(lynx_mvgam, series = 1, type = 'rootogram', n_bins = 25)
```

<img src="man/figures/README-unnamed-chunk-22-1.png" width="60%" style="display: block; margin: auto;" />

Finally look for any biases in predictions by examining a Probability
Integral Transform (PIT) histogram. If our predictions are not biased
one way or another (i.e. not consistently under- or over-predicting),
this histogram should look roughly uniform

``` r
ppc(lynx_mvgam, series = 1, type = 'pit')
```

<img src="man/figures/README-unnamed-chunk-23-1.png" width="60%" style="display: block; margin: auto;" />

All of these plots indicate the model is well calibrated against the
training data, with no apparent pathological behaviors exhibited. Have a
look at this model’s summary to see what is being estimated. Note that
no pathological behaviours have been detected and we achieve good
effective sample sizes / mixing for all parameters

``` r
summary(lynx_mvgam)
#> GAM formula:
#> population ~ s(season, bs = "cc", k = 12)
#> 
#> Family:
#> poisson
#> 
#> Link function:
#> log
#> 
#> Trend model:
#> AR3
#> 
#> N series:
#> 1
#> 
#> N timepoints:
#> 50
#> 
#> Status:
#> Fitted using Stan
#> 
#> GAM coefficient (beta) estimates:
#>                    2.5%         50%      97.5% Rhat n.eff
#> (Intercept)   6.7818392  6.80516000 6.82843550 1.00  3179
#> s(season).1  -0.5909618  0.04749685 0.73737175 1.00   752
#> s(season).2  -0.3533555  0.75292450 1.79455150 1.01   343
#> s(season).3  -0.1034166  1.12244000 2.40960925 1.02   242
#> s(season).4  -0.5529282  0.45162150 1.39613300 1.00   705
#> s(season).5  -1.1268045 -0.07493020 0.98803162 1.01   383
#> s(season).6  -1.0323980  0.09022960 1.09553275 1.01   544
#> s(season).7  -0.7160670  0.37072350 1.38456225 1.00   699
#> s(season).8  -0.9758658  0.12876900 1.72136250 1.01   354
#> s(season).9  -1.1490337 -0.37293850 0.58843957 1.01   486
#> s(season).10 -1.3958970 -0.69393450 0.04744051 1.00   575
#> 
#> GAM smoothing parameter (rho) estimates:
#>              2.5%      50%    97.5% Rhat n.eff
#> s(season) 2.20801 3.451535 4.262529 1.01   445
#> 
#> Latent trend AR parameter estimates:
#>                2.5%        50%      97.5% Rhat n.eff
#> ar1[1]    0.7194240  1.1298800 1.44770475 1.01   558
#> ar2[1]   -0.8406854 -0.4139430 0.06638206 1.01  1373
#> ar3[1]   -0.4980526 -0.1410060 0.25724135 1.00   554
#> sigma[1]  0.3986524  0.4986495 0.63420665 1.00  1163
#> 
#> Stan MCMC diagnostics
#> n_eff / iter looks reasonable for all parameters
#> Rhat looks reasonable for all parameters
#> 0 of 2000 iterations ended with a divergence (0%)
#> 0 of 2000 iterations saturated the maximum tree depth of 12 (0%)
#> E-FMI indicated no pathological behavior
#> 
```

As with any `MCMC` based software, we can inspect traceplots. Here for
the `GAM` component smoothing parameters. There is no requirement for
`rstan` to be installed as a dependency, but we can still use it if
available to examine traceplots

``` r
rstan::stan_trace(lynx_mvgam$model_output, 'rho')
```

<img src="man/figures/README-unnamed-chunk-25-1.png" width="60%" style="display: block; margin: auto;" />

and for the latent trend component parameters

``` r
rstan::stan_trace(lynx_mvgam$model_output, c('ar1', 'ar2', 'ar3', 'sigma'))
```

<img src="man/figures/README-unnamed-chunk-26-1.png" width="60%" style="display: block; margin: auto;" />

Inspect the model’s estimated smooth for the 19-year cyclic pattern,
which is shown as a ribbon plot of posterior empirical quantiles. We can
also overlay posterior quantiles of partial residuals (shown as ribbon
rectangles in red), which represent the leftover variation that the
model expects would remain if this smooth term was dropped but all other
parameters remained unchanged. Note that these are on a different scale
to those from `mgcv::plot.gam` as these are randomised quantile
residuals that are essentially standard normal in distribution. But
either way, a strong pattern in the partial residuals suggests there
would be strong patterns left unexplained in the model *if* we were to
drop this term, giving us further confidence that this function is
important in the model

``` r
plot(lynx_mvgam, type = 'smooths', residuals = TRUE)
```

<img src="man/figures/README-unnamed-chunk-27-1.png" width="60%" style="display: block; margin: auto;" />

It is often also useful to compare prior to posterior function
realisations to understand how informative the observed data have been
for learning these functional shapes

``` r
layout(matrix(1:2, nrow = 2))
plot(lynx_mvgam_prior, type = 'smooths', realisations = TRUE,
     n_realisations = 30)
```

<img src="man/figures/README-unnamed-chunk-28-1.png" width="60%" style="display: block; margin: auto;" />

``` r
plot(lynx_mvgam, type = 'smooths', realisations = TRUE,
     n_realisations = 30)
```

<img src="man/figures/README-unnamed-chunk-28-2.png" width="60%" style="display: block; margin: auto;" />

``` r
layout(1)
```

First derivatives of smooth functions can be plotted to inspect how the
slope of the function changes across its length. To plot these we use
the more flexible `plot_mvgam_smooth()` function

``` r
plot_mvgam_smooth(lynx_mvgam, 1, 'season', derivatives = TRUE)
```

<img src="man/figures/README-unnamed-chunk-29-1.png" width="60%" style="display: block; margin: auto;" />

We can also view the mvgam’s posterior retrodictions and predictions for
the entire series (testing and training)

``` r
plot(lynx_mvgam, type = 'forecast', newdata = lynx_test)
#> Out of sample DRPS:
#> [1] 1114.599
#> 
```

<img src="man/figures/README-unnamed-chunk-30-1.png" width="60%" style="display: block; margin: auto;" />

And the estimated latent trend component, again using the more flexible
`plot_mvgam_...()` option to show first derivatives of the estimated
trend

``` r
plot_mvgam_trend(lynx_mvgam, newdata = lynx_test, derivatives = TRUE)
```

<img src="man/figures/README-unnamed-chunk-31-1.png" width="60%" style="display: block; margin: auto;" />

We can also re-do the posterior predictive checks, but this time
focusing only on the out of sample period. This will give us better
insight into how the model is performing and whether it is able to
simulate realistic and unbiased future values

``` r
ppc(lynx_mvgam, series = 1, type = 'rootogram', newdata = lynx_test)
```

<img src="man/figures/README-unnamed-chunk-32-1.png" width="60%" style="display: block; margin: auto;" />

``` r
ppc(lynx_mvgam, series = 1, type = 'mean', newdata = lynx_test)
```

<img src="man/figures/README-unnamed-chunk-33-1.png" width="60%" style="display: block; margin: auto;" />

``` r
ppc(lynx_mvgam, series = 1, type = 'cdf', newdata = lynx_test)
```

<img src="man/figures/README-unnamed-chunk-34-1.png" width="60%" style="display: block; margin: auto;" />

``` r
ppc(lynx_mvgam, series = 1, type = 'pit', newdata = lynx_test)
```

<img src="man/figures/README-unnamed-chunk-35-1.png" width="60%" style="display: block; margin: auto;" />

A key aspect of ecological forecasting is to understand [how different
components of a model contribute to forecast
uncertainty](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/eap.1589).
We can estimate relative contributions to forecast uncertainty for the
GAM component and the latent trend component using `mvgam`

``` r
plot_mvgam_uncertainty(lynx_mvgam, newdata = lynx_test, legend_position = 'none')
text(1, 0.2, cex = 1.5, label="GAM component", 
     pos = 4, col="white", family = 'serif')
text(1, 0.8, cex = 1.5, label="Trend component", 
     pos = 4, col="#7C0000", family = 'serif')
```

<img src="man/figures/README-unnamed-chunk-36-1.png" width="60%" style="display: block; margin: auto;" />

Both components contribute to forecast uncertainty, suggesting we would
still need some more work to learn about factors driving the dynamics of
the system. But we will leave the model as-is for this example.
Diagnostics of the model can also be performed using `mvgam`. Have a
look at the model’s residuals, which are posterior empirical quantiles
of Dunn-Smyth randomised quantile residuals so should follow approximate
normality. We are primarily looking for a lack of autocorrelation, which
would suggest our AR3 model is appropriate for the latent trend

``` r
plot(lynx_mvgam, type = 'residuals')
```

<img src="man/figures/README-unnamed-chunk-37-1.png" width="60%" style="display: block; margin: auto;" />

## Comparing models based on forecasts

Another useful utility of `mvgam` is the ability to use approximate
rolling window forecasts to evaluate competing models that may represent
different hypotheses about the series dynamics. Here we will fit a
poorly specified model to showcase how these evaluations works. In this
model, we ignore the cyclic pattern of seasonality. We also use a random
walk process for the trend

``` r
lynx_mvgam_poor <- mvgam(data = lynx_train,
               newdata = lynx_test,
               formula = population ~ 1,
               family = 'poisson',
               trend_model = 'RW',
               use_stan = TRUE,
               chains = 4)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 3 finished in 1.0 seconds.
#> Chain 4 finished in 1.0 seconds.
#> Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 1 finished in 1.2 seconds.
#> Chain 2 finished in 1.1 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 1.1 seconds.
#> Total execution time: 1.2 seconds.
```

The first approximator targets each model’s ability to simulate temporal
dynamics using the single model that has been fit. We choose a set of
timepoints within the training data to forecast from, allowing us to
simulate a situation where the model’s parameters had already been
estimated but we have only observed data up to the evaluation timepoint
and would like to generate forecasts from the latent trends. Here we
simulate scenarios where we forecast ahead for the next 10 years. The
`compare_mvgams` function automates this process by rolling along a set
of timepoints for each model, ensuring a more in-depth evaluation of
each competing model at the same set of timepoints.

``` r
compare_mvgams(lynx_mvgam, lynx_mvgam_poor, fc_horizon = 10)
#> RPS summaries per model (lower is better)
#>             Min.  1st Qu.   Median     Mean   3rd Qu.      Max.
#> Model 1 2.708036 4.140680 4.324756 4.567742  4.998367  6.483963
#> Model 2 4.901111 6.972523 8.974301 9.217776 11.388963 13.812966
#> 
#> 90% interval coverages per model (closer to 0.9 is better)
#> Model 1 0.94 
#> Model 2 0.9
```

<img src="man/figures/README-unnamed-chunk-39-1.png" width="60%" style="display: block; margin: auto;" /><img src="man/figures/README-unnamed-chunk-39-2.png" width="60%" style="display: block; margin: auto;" /><img src="man/figures/README-unnamed-chunk-39-3.png" width="60%" style="display: block; margin: auto;" />

Summary statistics of the two models’ out of sample Discrete Rank
Probability Score (DRPS) indicate that the well-specified model performs
markedly better (lower DRPS) across most out of sample horizons.

The second approximator uses more conventional leave-future-out
comparisons. Time series models are often evaluated using an expanding
window training technique, where the model is initially trained on some
subset of data from `t = 1` to `t = n_train`, and then is used to
produce forecasts for the next `fc_horizon` time steps
`t = n_train + fc_horizon`. In the next iteration, the size of training
data is expanded by a single time point and the process repeated. This
is obviously computationally challenging for Bayesian time series
models, as the number of refits can be very large. `mvgam` uses an
approximation based on importance sampling. Briefly, we refit the model
using the first `min_t` observations to perform a single exact
`fc_horizon`-ahead forecast step. This forecast is evaluated against the
`min_t + fc_horizon` out of sample observations using the Expected Log
Predictive Density (ELPD). Next, we approximate each successive round of
expanding window forecasts by moving forward one step at a time
`for i in 1:N_evaluations` and re-weighting draws from the model’s
posterior predictive distribution using Pareto Smoothed Importance
Sampling (PSIS). In each iteration `i`, PSIS weights are obtained for
all observations that would have been included in the model if we had
re-fit. If these importance ratios are stable, we consider the
approximation adequate and use the re-weighted posterior’s forecast for
evaluating the next holdout set of testing observations
(`(min_t + i + 1):(min_t + i + fc_horizon)`). This is similar to the
process of particle filtering to update forecasts in light of new data
by re-weighting the posterior draws using importance weights. But at
some point the importance ratio variability will become too large and
importance sampling will be unreliable. This is indicated by the
estimated shape parameter `k` of the generalized Pareto distribution
crossing a certain threshold `pareto_k_threshold`. Only then do we refit
the model using all of the observations up to the time of the failure.
We then restart the process and iterate forward until the next refit is
triggered. The process is computationally much more efficient, as only a
fraction of the evaluations typically requires refits (the algorithm is
described in detail by Bürkner et al. 2020).

Paul-Christian Bürkner, Jonah Gabry & Aki Vehtari (2020). Approximate
leave-future-out cross-validation for Bayesian time series models.
Journal of Statistical Computation and Simulation. 90:14, 2499-2523.

For this example, we simulate a single count-valued time series of
length `T = 60`, using a latent `AR1` trend and a cyclic seasonal
pattern. Two models are fit as before, the first is a complex but more
correct model, while the second is simpler and mis-specified. We then
run approximate leave-future-out cross-validation, setting `min_t = 36`
so that the first refit uses all observations from `t = 1` to `t = 36`.
This is done for both models so that we can compare approximate ELPD
values when forecasting two time steps ahead (`fc_horizon = 2`)

``` r
set.seed(12345)
simdat <- sim_mvgam(T = 60, train_prop = 1, n_series = 1,
                    mu = 2,
                    trend_model = 'AR1', trend_rel = 0.35)
good <- mvgam(y ~ s(season, bs = 'cc', k = 8),
              trend_model = 'AR1',
              family = poisson(),
              data = simdat$data_train)
poor <- update(good, formula = y ~ 1,
               trend_model = 'RW')
lfo_good <- lfo_cv(good, min_t = 36,
                   fc_horizon = 2,
                   n_cores = 3)
lfo_poor <- lfo_cv(poor, min_t = 36,
                   fc_horizon = 2,
                   n_cores = 3)
```

The `S3` plotting function for these `lfo_cv` objects will show the
Pareto-k values and ELPD values over the evaluation time points. For the
Pareto-k plot, a dashed red line indicates the specified threshold
chosen for triggering model refits. For the ELPD plot, a dashed red line
indicates the bottom 10% quantile of ELPD values. Points below this
threshold may represent outliers that were more difficult to forecast

``` r
par(mar = c(4,4, 1, 1))
plot(lfo_good)
```

<img src="man/figures/README-unnamed-chunk-42-1.png" width="60%" style="display: block; margin: auto;" />

``` r
par(mar = c(4,4, 1, 1))
plot(lfo_poor)
```

<img src="man/figures/README-unnamed-chunk-43-1.png" width="60%" style="display: block; margin: auto;" />

The model with the better ELPD values (higher values are better in this
case) should be preferred. First we can calculate the proportion of
forecast time points in which the first model gives better forecasts
than the poor model

``` r
length(which((lfo_good$elpds - lfo_poor$elpds) > 0)) /
  length(lfo_good$elpds)
#> [1] 0.8181818
```

Total ELPDs per model are also a useful overall indicator of performance

``` r
lfo_good$sum_ELPD
#> [1] -125.4208
lfo_poor$sum_ELPD
#> [1] -140.8736
```

As before, these metrics all favour the more complex model over the
simpler “poor” model. This gives us confidence that the more complex
model will perform better in future forecasting exercises. The first
approximator is faster as it does not require refits, but caution is
needed as the state of the latent trend at the evaluation time point has
already been informed by both the past and future observations. The
second approximator, using PSIS for approximate leave-future-out, should
be preferred when computationally accessible.

## Extended observation families

`mvgam` was originally designed to analyse and forecast non-negative
integer-valued data (counts). These data are traditionally challenging
to analyse with existing time-series analysis packages. But further
development of `mvgam` has resulted in support for a growing number of
observation families that extend to other types of data. Currently, the
package can handle data for the following families:

- `gaussian()` for real-valued data
- `student_t()` for heavy-tailed real-valued data
- `lognormal()` for non-negative real-valued data
- `betar()` for proportional data on `(0,1)`
- `poisson()` for count data
- `nb()` for overdispersed count data
- `tweedie()` for overdispersed count data

Note that only `poisson()`, `nb()`, and `tweedie()` are available if
using `JAGS`. All families, apart from `tweedie()`, are supported if
using `Stan`. See `??mvgam_families` for more information. Below is a
simple example for simulating and modelling proportional data with
`Beta` observations over a set of seasonal series with independent
Gaussian Process dynamic trends:

``` r
set.seed(100)
data <- sim_mvgam(family = betar(),
                 T = 80,
                 trend_model = 'GP',
                 trend_rel = 0.5, 
                 seasonality = 'shared')
plot_mvgam_series(data = data$data_train, series = 'all')
```

<img src="man/figures/README-beta_sim-1.png" width="60%" style="display: block; margin: auto;" />

``` r
mod <- mvgam(y ~ s(season, bs = 'cc', k = 7) +
               s(season, by = series, m = 1, k = 5),
             trend_model = 'GP',
             data = data$data_train,
             newdata = data$data_test,
             family = betar())
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 4 finished in 9.7 seconds.
#> Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 2 finished in 10.1 seconds.
#> Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 3 finished in 10.1 seconds.
#> Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 1 finished in 10.6 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 10.1 seconds.
#> Total execution time: 10.7 seconds.
```

Inspect the summary to see that the posterior now also contains
estimates for the `Beta` precision parameters $\phi$

``` r
summary(mod)
#> GAM formula:
#> y ~ s(season, bs = "cc", k = 7) + s(season, by = series, m = 1, 
#>     k = 5)
#> 
#> Family:
#> beta
#> 
#> Link function:
#> logit
#> 
#> Trend model:
#> GP
#> 
#> N series:
#> 3
#> 
#> N timepoints:
#> 60
#> 
#> Status:
#> Fitted using Stan
#> 
#> Precision parameter estimates:
#>            2.5%      50%    97.5% Rhat n.eff
#> phi[1] 4.876074 7.098455 10.17424    1  1956
#> phi[2] 5.099641 7.700575 11.00165    1  2015
#> phi[3] 4.674072 6.883865  9.63730    1  2099
#> 
#> GAM coefficient (beta) estimates:
#>                                   2.5%           50%       97.5% Rhat n.eff
#> (Intercept)                -0.04773704  0.1968735000  0.39843275 1.00   969
#> s(season).1                -0.49098633 -0.2091850000  0.05071321 1.00  2139
#> s(season).2                -0.97145335 -0.6910905000 -0.41763498 1.00  1465
#> s(season).3                -1.15527975 -0.8483775000 -0.56043315 1.00  1737
#> s(season).4                -0.53083210 -0.2576320000  0.03717659 1.00  1470
#> s(season).5                 0.08364336  0.3390515000  0.62866190 1.00  1726
#> s(season):seriesseries_1.1 -0.25870350 -0.0345460500  0.15249382 1.00  1250
#> s(season):seriesseries_1.2 -0.07397830 -0.0008982415  0.07402040 1.00  1939
#> s(season):seriesseries_1.3 -0.11284960 -0.0064969750  0.09576438 1.00  1624
#> s(season):seriesseries_1.4 -0.21944107 -0.0166044000  0.16996480 1.00  1505
#> s(season):seriesseries_2.1 -0.09691727  0.0828633500  0.40074885 1.01   689
#> s(season):seriesseries_2.2 -0.07848547  0.0008876460  0.08535670 1.00  1323
#> s(season):seriesseries_2.3 -0.08597769  0.0170877500  0.14209847 1.00  1401
#> s(season):seriesseries_2.4 -0.16160547  0.0191160500  0.27116510 1.00  1080
#> s(season):seriesseries_3.1 -0.17881935  0.0267890500  0.25312230 1.00   951
#> s(season):seriesseries_3.2 -0.06275557  0.0101814000  0.09231602 1.00  1611
#> s(season):seriesseries_3.3 -0.07222653  0.0279952000  0.15251422 1.00  1417
#> s(season):seriesseries_3.4 -0.22683805 -0.0267157000  0.16414105 1.00  1137
#> 
#> GAM smoothing parameter (rho) estimates:
#>                              2.5%      50%    97.5% Rhat n.eff
#> s(season)                2.034515 3.321820 4.140010    1  2085
#> s(season):seriesseries_1 1.617509 3.325800 4.203815    1  1006
#> s(season):seriesseries_2 1.108337 3.162870 4.214624    1   684
#> s(season):seriesseries_3 1.552668 3.250025 4.226449    1   942
#> 
#> Latent trend marginal deviation (alpha) and length scale (rho) estimates:
#>                   2.5%       50%      97.5% Rhat n.eff
#> alpha_gp[1] 0.28276395  0.578943  1.0635117    1  1136
#> alpha_gp[2] 0.37753145  0.699841  1.2652760    1  1263
#> alpha_gp[3] 0.08258895  0.382536  0.8951205    1  1085
#> rho_gp[1]   1.64042850  6.187095 23.5220850    1   813
#> rho_gp[2]   2.87517500 21.540700 62.1336925    1   510
#> rho_gp[3]   1.33655400  7.135475 52.3140475    1   616
#> 
#> Stan MCMC diagnostics
#> n_eff / iter looks reasonable for all parameters
#> Rhat looks reasonable for all parameters
#> 26 of 2000 iterations ended with a divergence (1.3%)
#> *Try running with larger adapt_delta to remove the divergences
#> 0 of 2000 iterations saturated the maximum tree depth of 12 (0%)
#> E-FMI indicated no pathological behavior
#> 
```

Plot the hindcast and forecast distributions for one series

``` r
plot(mod, type = 'forecast', newdata = data$data_test, series = 2)
#> Out of sample CRPS:
#> [1] 1.874611
#> 
```

<img src="man/figures/README-beta_fc-1.png" width="60%" style="display: block; margin: auto;" />

## Dynamic coefficient models

Dynamic fixed-effect coefficients (often referred to as dynamic linear
models) can also be readily incorporated into GAMs / DGAMs. In `mvgam`,
the `dynamic()` formula wrapper is used to set these up. The plan is to
incorporate a range of dynamic options (such as random walk, AR1 etc…)
but for the moment only low-rank Gaussian Process smooths are allowed
(making use of the `gp` basis in `mgcv`). An example below illustrates:

Simulate a time-varying coefficient using a squared exponential Gaussian
Process function with length scale $\rho$=10

``` r
set.seed(1111)
N = 200
beta_temp <- mvgam:::sim_gp(rnorm(1),
                            alpha_gp = 0.75,
                            rho_gp = 10,
                            h = N) + 0.5
plot(beta_temp, type = 'l', lwd = 3, 
     bty = 'l', xlab = 'Time', ylab = 'Coefficient',
     col = 'darkred')
box(bty = 'l', lwd = 2)
```

<img src="man/figures/README-unnamed-chunk-48-1.png" width="60%" style="display: block; margin: auto;" />

Now simulate the outcome, which is a Gaussian observation process (with
observation error) over the time-varying effect of $temperature$

``` r
temp <- rnorm(N, sd = 1)
out <- rnorm(N, mean = 4 + beta_temp * temp,
             sd = 0.25)
time <- seq_along(temp)
plot(out,  type = 'l', lwd = 3, 
     bty = 'l', xlab = 'Time', ylab = 'Outcome',
     col = 'darkred')
box(bty = 'l', lwd = 2)
```

<img src="man/figures/README-unnamed-chunk-49-1.png" width="60%" style="display: block; margin: auto;" />

Gather the data into a `data.frame` and fit a model using the
`dynamic()` formula wrapper to specify a low-rank Gaussian Process
smooth function to estimate the time-varying coefficient of
$temperature$. We will mis-specify the $\rho$ parameter here as, in
practice, it is never known

``` r
data = data.frame(out, temp, time)
data_train <- data[1:190,]
data_test <- data[191:200,]
mod <- mvgam(out ~ dynamic(temp, rho = 8, stationary = TRUE),
             family = gaussian(),
             data = data_train)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 1 finished in 1.2 seconds.
#> Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 4 finished in 1.2 seconds.
#> Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 2 finished in 1.3 seconds.
#> Chain 3 finished in 1.3 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 1.2 seconds.
#> Total execution time: 1.4 seconds.
```

Inspect the model summary, which now contains estimates for the
observation errors

``` r
summary(mod)
#> GAM formula:
#> out ~ dynamic(temp, rho = 8, stationary = TRUE)
#> 
#> Family:
#> gaussian
#> 
#> Link function:
#> identity
#> 
#> Trend model:
#> None
#> 
#> N series:
#> 1
#> 
#> N timepoints:
#> 190
#> 
#> Status:
#> Fitted using Stan
#> 
#> Observation error parameter estimates:
#>                   2.5%       50%     97.5% Rhat n.eff
#> sigma_obs[1] 0.2202185 0.2450235 0.2734452    1  2275
#> 
#> GAM coefficient (beta) estimates:
#>                        2.5%           50%         97.5% Rhat n.eff
#> (Intercept)      3.93307950  3.9656150000  3.9997657500 1.00  2512
#> s(time):temp.1  -0.74474050 -0.2304230000  0.1737626750 1.01   285
#> s(time):temp.2   0.04845902  0.0891076000  0.1304172500 1.00  1994
#> s(time):temp.3  -0.12117113  0.0512687500  0.1870407500 1.01   309
#> s(time):temp.4   0.13330525  0.1796355000  0.2257777000 1.00  2040
#> s(time):temp.5  -0.54095685 -0.4373505000 -0.3206291500 1.01   362
#> s(time):temp.6   0.10784913  0.1578515000  0.2079530250 1.00  1870
#> s(time):temp.7  -0.26997520 -0.1753575000 -0.0907634275 1.01   366
#> s(time):temp.8  -0.73046917 -0.6787185000 -0.6306463250 1.00  1993
#> s(time):temp.9   0.05617914  0.1338275000  0.2259883000 1.00   516
#> s(time):temp.10  0.23238117  0.2928780000  0.3533036750 1.00  1801
#> s(time):temp.11 -0.23112530 -0.1439430000 -0.0592890025 1.01   527
#> s(time):temp.12  0.17742067  0.2505315000  0.3238529000 1.00  2022
#> s(time):temp.13 -0.23923745 -0.1390320000 -0.0338093775 1.00   563
#> s(time):temp.14 -0.05362894  0.0370395000  0.1264298500 1.00  1725
#> s(time):temp.15 -0.29450673 -0.1732645000 -0.0469807575 1.00   812
#> s(time):temp.16  0.01284141  0.1213115000  0.2332133250 1.00  1738
#> s(time):temp.17 -0.14467292 -0.0069032300  0.1361887750 1.00   957
#> s(time):temp.18  0.02979525  0.1649075000  0.3033235000 1.00  2049
#> s(time):temp.19 -0.08465953  0.0875976000  0.2631511250 1.00  1238
#> s(time):temp.20 -0.13092280  0.0507060500  0.2269631250 1.00  2294
#> s(time):temp.21 -0.10496045  0.1276110000  0.3531367000 1.00  1600
#> s(time):temp.22 -0.51660520 -0.2576350000 -0.0211089650 1.00  1820
#> s(time):temp.23 -0.44104567 -0.1388530000  0.1672533750 1.00  1875
#> s(time):temp.24 -0.70089613 -0.3573005000 -0.0007156916 1.00  2103
#> s(time):temp.25 -0.37327505  0.0005138345  0.3538798250 1.00  1697
#> s(time):temp.26 -0.48196535 -0.0797231500  0.2993224750 1.00  1950
#> s(time):temp.27 -0.75415372 -0.1791245000  0.2784030750 1.01   286
#> 
#> GAM smoothing parameter (rho) estimates:
#>                    2.5%       50%      97.5% Rhat n.eff
#> s(time):temp  -1.665645 -1.370235 -0.9980222 1.00  1442
#> s(time):temp2 -2.817081  1.088455  3.6937990 1.01  1165
#> 
#> Stan MCMC diagnostics
#> n_eff / iter looks reasonable for all parameters
#> Rhat looks reasonable for all parameters
#> 4 of 2000 iterations ended with a divergence (0.2%)
#> *Try running with larger adapt_delta to remove the divergences
#> 0 of 2000 iterations saturated the maximum tree depth of 12 (0%)
#> E-FMI indicated no pathological behavior
#> 
```

Plot the estimated time-varying coefficient for the in-sample training
period

``` r
plot(mod, type = 'smooths')
```

<img src="man/figures/README-unnamed-chunk-52-1.png" width="60%" style="display: block; margin: auto;" />

Plot the estimates for the in-sample and out-of-sample periods to see
how the Gaussian Process function produces sensible smooth forecasts.
Overlay the true simulated function to see that the model has adequately
estimated it’s dynamics in both the training and testing data partitions

``` r
plot_mvgam_smooth(mod, smooth = 1, newdata = data)
abline(v = 190, lty = 'dashed', lwd = 2)
lines(beta_temp, lwd = 2.5, col = 'white')
lines(beta_temp, lwd = 2)
```

<img src="man/figures/README-unnamed-chunk-53-1.png" width="60%" style="display: block; margin: auto;" />

This results in sensible forecasts of the observations as well

``` r
plot(mod, type = 'forecast', newdata = data_test)
#> Out of sample CRPS:
#> [1] 1.738253
#> 
```

<img src="man/figures/README-unnamed-chunk-54-1.png" width="60%" style="display: block; margin: auto;" />

There are many more extended uses for `mvgam` models, including the
ability to fit dynamic factor processes for analysing and forecasting
sets of multivariate time series. See the package documentation for more
details.

## License

This project is licensed under an `MIT` open source license