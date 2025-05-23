% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_mvgam_trend.R
\name{plot_mvgam_trend}
\alias{plot_mvgam_trend}
\title{Plot latent trend predictions from \pkg{mvgam} models}
\usage{
plot_mvgam_trend(
  object,
  series = 1,
  newdata,
  data_test,
  realisations = FALSE,
  n_realisations = 15,
  n_cores = 1,
  derivatives = FALSE,
  xlab,
  ylab
)
}
\arguments{
\item{object}{\code{list} object returned from \code{mvgam}. See \code{\link[=mvgam]{mvgam()}}}

\item{series}{\code{integer} specifying which series in the set is to be plotted}

\item{newdata}{Optional \code{dataframe} or \code{list} of test data containing at least 'series' and 'time'
in addition to any other variables included in the linear predictor of the original \code{formula}.}

\item{data_test}{Deprecated. Still works in place of \code{newdata} but users are recommended to use
\code{newdata} instead for more seamless integration into \code{R} workflows}

\item{realisations}{\code{logical}. If \code{TRUE}, posterior trend realisations are shown as a spaghetti plot,
making it easier to visualise the diversity of possible trend paths. If \code{FALSE}, the default,
empirical quantiles of the posterior distribution are shown}

\item{n_realisations}{\code{integer} specifying the number of posterior realisations to plot, if
\code{realisations = TRUE}. Ignored otherwise}

\item{n_cores}{Deprecated. Parallel processing is no longer supported}

\item{derivatives}{\code{logical}. If \code{TRUE}, an additional plot will be returned to show the
estimated 1st derivative for the estimated trend}

\item{xlab}{label for x axis.}

\item{ylab}{label for y axis.}
}
\value{
A \code{ggplot} object
}
\description{
Plot latent trend predictions from \pkg{mvgam} models
}
\examples{
\donttest{
simdat <- sim_mvgam(n_series = 3, trend_model = 'AR1')
mod <- mvgam(y ~ s(season, bs = 'cc', k = 6),
            trend_model = AR(),
            noncentred = TRUE,
            data = simdat$data_train,
            chains = 2)

# Plot estimated trends for some series
plot_mvgam_trend(mod)
plot_mvgam_trend(mod, series = 2)

# Extrapolate trends forward in time and plot on response scale
plot_mvgam_trend(mod, newdata = simdat$data_test)
plot_mvgam_trend(mod, newdata = simdat$data_test, series = 2)

# But it is recommended to compute extrapolations for all series
# first and then plot
trend_fc <- forecast(mod, newdata = simdat$data_test)
plot(trend_fc, series = 1)
plot(trend_fc, series = 2)
}
}
