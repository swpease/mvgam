% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_mvgam_randomeffects.R
\name{plot_mvgam_randomeffects}
\alias{plot_mvgam_randomeffects}
\title{Plot random effect terms from \pkg{mvgam} models}
\usage{
plot_mvgam_randomeffects(object, trend_effects = FALSE)
}
\arguments{
\item{object}{\code{list} object of class \code{mvgam}. See \code{\link[=mvgam]{mvgam()}}}

\item{trend_effects}{logical. If \code{TRUE} and a \code{trend_formula} was used in model
fitting, terms from the trend (i.e. process) model will be plotted}
}
\value{
A base \code{R} graphics plot
}
\description{
This function plots posterior empirical quantiles for random effect smooths (bs = re)
}
\details{
Posterior empirical quantiles of random effect coefficient estimates
(on the link scale) are calculated and visualised as ribbon plots.
Labels for coefficients are taken from the levels of the original factor variable
that was used to specify the smooth in the model's formula
}
