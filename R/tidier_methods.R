#' @importFrom generics augment
#' @export
generics::augment

#' Augment an mvgam object's data
#'
#' Implements the generic `augment` from the package {broom}.
#' Add fits and residuals to the data, returning a `tibble`.
#'
#' @param x An object of class `mvgam`.
#' @param robust If `FALSE` (the default) the mean is used as the measure of
#' central tendency and the standard deviation as the measure of variability.
#' If `TRUE`, the median and the median absolute deviation (MAD)
#' are applied instead.
#' @param probs The percentiles to be computed by the quantile function.
#' @param ... Unused, included for generic consistency only.
#' @returns A `tibble` combining:
#'   * The data supplied to `mvgam()`.
#'   * The fitted backcasts, along with their variability and credible bounds.
#'   * The residuals, along with their variability and credible bounds.
#'
#' @examples
#' \dontrun{
#' set.seed(0)
#' dat <- sim_mvgam(T = 80,
#'                  n_series = 3,
#'                  mu = 2,
#'                  trend_model = AR(p = 1),
#'                  prop_missing = 0.1,
#'                  prop_trend = 0.6)
#' mod1 <- mvgam(formula = y ~ s(season, bs = 'cc', k = 6),
#'               data = dat$data_train,
#'               trend_model = AR(),
#'               family = poisson(),
#'               noncentred = TRUE)
#' augment(mod1, robust = TRUE, probs = c(0.25, 0.75))
#' }
#'
#' @importFrom stats residuals
#' @export
augment.mvgam <- function(x,
                          robust = FALSE,
                          probs = c(0.025, 0.975),
                          ...) {
  obs_data = tibble::as_tibble(x$obs_data) %>%
    dplyr::mutate(.observed = y) %>%
    dplyr::select(!dplyr::any_of(c("index..orig..order", "index..time..index")))
  resids = residuals(x, robust = robust, probs = probs) %>%
    tibble::as_tibble()
  fits = fitted(x, robust = robust, probs = probs) %>%
    tibble::as_tibble()
  hc_fits = fits %>%
    dplyr::slice_head(n = NROW(resids))  # fits can include fcs

  colnames(resids) <- c(".resid", ".resid.variability", ".resid.cred.low", ".resid.cred.high")
  colnames(hc_fits) <- c(".fitted", ".fit.variability", ".fit.cred.low", ".fit.cred.high")

  augmented = dplyr::bind_cols(obs_data, hc_fits, resids)

  augmented
}
