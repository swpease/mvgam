#' Helper functions for \pkg{marginaleffects} calculations in \pkg{mvgam} models
#' @importFrom stats coef model.frame
#' @importFrom insight find_predictors get_data
#' @importFrom marginaleffects get_coef set_coef get_vcov get_predict
#' @importFrom utils getFromNamespace
#' @inheritParams marginaleffects::get_coef
#' @inheritParams marginaleffects::set_coef
#' @inheritParams marginaleffects::get_vcov
#' @inheritParams marginaleffects::get_predict
#' @inheritParams insight::get_data
#' @inheritParams insight::find_predictors
#' @param trend_effects `logical`, extract from the process model component
#' (only applicable if a `trend_formula` was specified in the model)
#' @param process_error `logical`. If `TRUE`, uncertainty in the latent
#' process (or trend) model is incorporated in predictions
#' @return Objects suitable for internal 'marginaleffects' functions to proceed.
#' See [marginaleffects::get_coef()], [marginaleffects::set_coef()],
#' [marginaleffects::get_vcov()], [marginaleffects::get_predict()],
#' [insight::get_data()] and [insight::find_predictors()] for details
#' @name mvgam_marginaleffects
#' @author Nicholas J Clark
NULL

#' @export
#' @importFrom marginaleffects predictions
marginaleffects::predictions

#' @export
#' @importFrom marginaleffects avg_predictions
marginaleffects::avg_predictions

#' @export
#' @importFrom marginaleffects plot_predictions
marginaleffects::plot_predictions

#' @export
#' @importFrom marginaleffects slopes
marginaleffects::slopes

#' @export
#' @importFrom marginaleffects plot_slopes
marginaleffects::plot_slopes

#' @export
#' @importFrom marginaleffects comparisons
marginaleffects::comparisons

#' @export
#' @importFrom marginaleffects plot_comparisons
marginaleffects::plot_comparisons

#' @export
#' @importFrom marginaleffects datagrid
marginaleffects::datagrid

#' @export
#' @importFrom marginaleffects hypotheses
marginaleffects::hypotheses

#' @export
#' @importFrom marginaleffects get_predict
marginaleffects::get_predict

#' @export
#' @importFrom insight get_data
insight::get_data

#' Functions needed for working with \pkg{marginaleffects}
#' @rdname mvgam_marginaleffects
#' @export
get_coef.mvgam <- function(model, trend_effects = FALSE, ...) {
  # Check trend_effects
  if (trend_effects) {
    if (is.null(model$trend_call)) {
      stop('no trend_formula exists so there no trend-level coefficients')
    }
  }

  if (!trend_effects) {
    b <- coef(model$mgcv_model)
  } else {
    b <- coef(model$trend_mgcv_model)
  }
  return(b)
}

#' @rdname mvgam_marginaleffects
#' @export
set_coef.mvgam <- function(model, coefs, trend_effects = FALSE, ...) {
  # Check trend_effects
  if (trend_effects) {
    if (is.null(model$trend_call)) {
      stop('no trend_formula exists so there no trend-level coefficients')
    }
  }

  out <- model

  if (!trend_effects) {
    out$mgcv_model$coefficients <- coefs
  } else {
    out$trend_mgcv_model$coefficients <- coefs
  }
  return(out)
}

#' @rdname mvgam_marginaleffects
#' @export
get_vcov.mvgam <- function(model, vcov = NULL, ...) {
  if (!is.null(vcov) && !is.logical(vcov)) {
    insight::format_warning(
      "The `vcov` argument is not supported for models of this class."
    )
  }
  return(NULL)
}

#' @rdname mvgam_marginaleffects
#' @export
get_predict.mvgam <- function(
  model,
  newdata,
  type = 'response',
  process_error = FALSE,
  ...
) {
  preds <- predict(
    object = model,
    newdata = newdata,
    type = type,
    process_error = process_error,
    summary = FALSE,
    ...
  )
  if ("rowid" %in% colnames(newdata)) {
    out <- data.frame(
      rowid = newdata[["rowid"]],
      estimate = apply(preds, 2, median)
    )
  } else {
    out <- data.frame(
      rowid = seq_len(NCOL(preds)),
      estimate = apply(preds, 2, median)
    )
  }

  attr(out, "posterior_draws") <- t(preds)
  return(out)
}

#' Functions needed for getting data / objects with \pkg{insight}
#' @rdname mvgam_marginaleffects
#' @export
get_data.mvgam = function(x, source = "environment", verbose = TRUE, ...) {
  resp_terms <- as.character(rlang::f_lhs(x$call))
  if (length(resp_terms) == 1L) {
    resp <- resp_terms
  } else {
    if (any(grepl('cbind', resp_terms))) {
      resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
      resp <- resp_terms[1]
    }
  }

  mf <- tryCatch(
    {
      # Drop response observations if a trend call was used because often
      # there won't be an easy way to match them up (for example if multiple
      # series depend on a shared latent trend)
      if (!is.null(x$trend_call)) {
        # Original series, time and outcomes
        orig_dat <- data.frame(
          series = x$obs_data$series,
          time = x$obs_data$index..time..index,
          y = x$obs_data$y
        )

        # Add indicators of trend names as factor levels using the trend_map
        trend_indicators <- vector(length = length(orig_dat$time))
        for (i in 1:length(orig_dat$time)) {
          trend_indicators[i] <- x$trend_map$trend[which(
            x$trend_map$series == orig_dat$series[i]
          )]
        }
        trend_indicators <- factor(
          paste0('trend', trend_indicators),
          levels = paste0('trend', unique(x$trend_map$trend))
        )

        # Trend-level data, before any slicing that took place
        orig_dat %>%
          dplyr::bind_cols(data.frame(
            trend_series = trend_indicators,
            row_num = 1:length(x$obs_data$index..time..index)
          )) -> trend_level_data

        # # We only kept one time observation per trend
        trend_level_data %>%
          dplyr::group_by(trend_series, time) %>%
          dplyr::slice_head(n = 1) %>%
          dplyr::pull(row_num) -> idx

        # Extract model.frame for trend_level effects and add the
        # trend indicators
        mf_data <- model.frame(x, trend_effects = TRUE)
        mf_data$trend_series <- trend_level_data$trend_series[idx]
        mf_data$time <- trend_level_data$time[idx]

        if ('series' %in% names(mf_data)) {
          mf_data %>%
            dplyr::select(-series) -> mf_data
        }

        # Now join with the original data so the original observations can
        # be included
        trend_level_data %>%
          dplyr::left_join(mf_data, by = c('trend_series', 'time')) %>%
          dplyr::select(-trend_series, -row_num, -trend_y) -> mf_data

        # Extract any predictors from the observation level model and
        # bind to the trend level model.frame
        mf_obs <- model.frame(x, trend_effects = FALSE)
        mf_data <- cbind(mf_obs, mf_data) %>%
          subset(., select = which(!duplicated(names(.))))

        # Now get the observed response, in case there are any
        # NAs there that need to be updated
        mf_data[, resp] <- x$obs_data$y
      } else {
        mf_data <- model.frame(x, trend_effects = FALSE)
        mf_data[, resp] <- x$obs_data[[resp]]
      }
      mf_data
    },
    error = function(x) {
      NULL
    }
  )

  prep_data <- utils::getFromNamespace(".prepare_get_data", "insight")
  out <- prep_data(x, mf, effects = "all", verbose = verbose)

  # Remove colnames with cbind in them as these can be artifacts in
  # binomial model data preparations
  if (any(grepl('cbind', colnames(out)))) {
    out %>%
      dplyr::select(-matches('cbind')) -> out
  }

  return(out)
}

#' @rdname mvgam_marginaleffects
#' @export
get_data.mvgam_prefit = function(
  x,
  source = "environment",
  verbose = TRUE,
  ...
) {
  resp_terms <- as.character(rlang::f_lhs(x$call))
  if (length(resp_terms) == 1L) {
    resp <- resp_terms
  } else {
    if (any(grepl('cbind', resp_terms))) {
      resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
      resp <- resp_terms[1]
    }
  }

  mf <- tryCatch(
    {
      # Drop response observations if a trend call was used because often
      # there won't be an easy way to match them up (for example if multiple
      # series depend on a shared latent trend)
      if (!is.null(x$trend_call)) {
        # Original series, time and outcomes
        orig_dat <- data.frame(
          series = x$obs_data$series,
          time = x$obs_data$index..time..index,
          y = x$obs_data$y
        )

        # Add indicators of trend names as factor levels using the trend_map
        trend_indicators <- vector(length = length(orig_dat$time))
        for (i in 1:length(orig_dat$time)) {
          trend_indicators[i] <- x$trend_map$trend[which(
            x$trend_map$series == orig_dat$series[i]
          )]
        }
        trend_indicators <- as.factor(paste0('trend', trend_indicators))

        # Trend-level data, before any slicing that took place
        trend_level_data <- data.frame(
          trend_series = trend_indicators,
          series = orig_dat$series,
          time = orig_dat$time,
          y = orig_dat$y,
          row_num = 1:length(x$obs_data$index..time..index)
        )

        # # We only kept one time observation per trend
        trend_level_data %>%
          dplyr::group_by(trend_series, time) %>%
          dplyr::slice_head(n = 1) %>%
          dplyr::pull(row_num) -> idx

        # Extract model.frame for trend_level effects and add the
        # trend indicators
        mf_data <- model.frame(x, trend_effects = TRUE)
        mf_data$trend_series <- trend_level_data$trend_series[idx]
        mf_data$time <- trend_level_data$time[idx]

        if ('series' %in% names(mf_data)) {
          mf_data %>%
            dplyr::select(-series) -> mf_data
        }

        # Now join with the original data so the original observations can
        # be included
        trend_level_data %>%
          dplyr::left_join(mf_data, by = c('trend_series', 'time')) %>%
          dplyr::select(-trend_series, -row_num, -trend_y) -> mf_data

        # Extract any predictors from the observation level model and
        # bind to the trend level model.frame
        mf_obs <- model.frame(x, trend_effects = FALSE)
        mf_data <- cbind(mf_obs, mf_data) %>%
          subset(., select = which(!duplicated(names(.))))

        # Now get the observed response, in case there are any
        # NAs there that need to be updated
        mf_data[, resp] <- x$obs_data$y
      } else {
        mf_data <- model.frame(x, trend_effects = FALSE)
        mf_data[, resp] <- x$obs_data[[resp]]
      }
      mf_data
    },
    error = function(x) {
      NULL
    }
  )

  prep_data <- utils::getFromNamespace(".prepare_get_data", "insight")
  out <- prep_data(x, mf, effects = "all", verbose = verbose)

  # Remove colnames with cbind in them as these can be artifacts in
  # binomial model data preparations
  if (any(grepl('cbind', colnames(out)))) {
    out %>%
      dplyr::select(-matches('cbind')) -> out
  }

  return(out)
}

#' @rdname mvgam_marginaleffects
#' @export
find_predictors.mvgam = function(
  x,
  effects = c('fixed', 'random', 'all'),
  component = c(
    'all',
    'conditional',
    'zi',
    'zero_inflated',
    'dispersion',
    'instruments',
    'correlation',
    'smooth_terms'
  ),
  flatten = FALSE,
  verbose = TRUE,
  ...
) {
  obs_preds <- insight::find_predictors(
    x$mgcv_model,
    effects = effects,
    component = component,
    flatten = flatten
  )
  if (!is.null(x$trend_call)) {
    preds <- list()
    trend_preds <- insight::find_predictors(
      x$trend_mgcv_model,
      effects = effects,
      component = component,
      flatten = flatten
    )

    if (flatten) {
      preds <- unique(c(obs_preds, trend_preds))
    } else {
      preds$conditional <- unique(c(
        obs_preds$conditional,
        trend_preds$conditional
      ))
    }
  } else {
    preds <- obs_preds
  }

  if (x$family == 'nmix') {
    if (flatten) {
      preds <- c(preds, 'cap')
    } else {
      preds$conditional <- c(preds$conditional, 'cap')
    }
  }

  # Check for offsets and add appropriately
  if (!is.null(attr(terms(x$call), "offset"))) {
    off_names <- grep(
      'offset',
      rownames(attr(terms.formula(x$call), 'factors')),
      value = TRUE
    )
    off_names <- sub(
      "offset\\((.*)\\)$",
      "\\1",
      grep('offset', off_names, value = TRUE)
    )

    if (flatten) {
      for (i in 1:length(off_names)) {
        preds <- c(preds, off_names[i])
      }
    } else {
      for (i in 1:length(off_names)) {
        preds$conditional <- c(preds$conditional, off_names[i])
      }
    }
  }

  # Any other required variables, needed for grouped models
  if (!inherits(attr(x$model_data, 'trend_model'), 'mvgam_trend')) {
    trend_model <- list(
      trend_model = attr(x$model_data, 'trend_model'),
      unit = 'time',
      gr = 'NA',
      subgr = 'series'
    )
  } else {
    trend_model <- attr(x$model_data, 'trend_model')
  }
  other_vars <- c(trend_model$unit, trend_model$gr, trend_model$subgr)
  if (!is.null(attr(x$model_data, 'prepped_trend_model'))) {
    prepped_model <- attr(x$model_data, 'prepped_trend_model')
    other_vars <- c(
      other_vars,
      c(prepped_model$unit, prepped_model$gr, prepped_model$subgr)
    )
  }

  if (flatten) {
    other_vars <- setdiff(unique(other_vars), c('NA', preds))
    preds <- c(preds, other_vars)
  } else {
    other_vars <- setdiff(unique(other_vars), c('NA', preds$conditional))
    preds$conditional <- c(preds$conditional, other_vars)
  }

  return(preds)
}

#' @rdname mvgam_marginaleffects
#' @export
find_predictors.mvgam_prefit = function(
  x,
  effects = c('fixed', 'random', 'all'),
  component = c(
    'all',
    'conditional',
    'zi',
    'zero_inflated',
    'dispersion',
    'instruments',
    'correlation',
    'smooth_terms'
  ),
  flatten = FALSE,
  verbose = TRUE,
  ...
) {
  obs_preds <- insight::find_predictors(
    x$mgcv_model,
    effects = effects,
    component = component,
    flatten = flatten
  )
  if (!is.null(x$trend_call)) {
    preds <- list()
    trend_preds <- insight::find_predictors(
      x$trend_mgcv_model,
      effects = effects,
      component = component,
      flatten = flatten
    )

    if (flatten) {
      preds <- unique(c(obs_preds, trend_preds))
    } else {
      preds$conditional <- unique(c(
        obs_preds$conditional,
        trend_preds$conditional
      ))
    }
  } else {
    preds <- obs_preds
  }

  if (x$family == 'nmix') {
    if (flatten) {
      preds <- c(preds, 'cap')
    } else {
      preds$conditional <- c(preds$conditional, 'cap')
    }
  }

  # Any other required variables, needed for grouped models
  if (!inherits(attr(x$model_data, 'trend_model'), 'mvgam_trend')) {
    trend_model <- list(
      trend_model = attr(x$model_data, 'trend_model'),
      unit = 'time',
      gr = 'NA',
      subgr = 'series'
    )
  } else {
    trend_model <- attr(x$model_data, 'trend_model')
  }
  other_vars <- c(trend_model$unit, trend_model$gr, trend_model$subgr)
  if (!is.null(attr(x$model_data, 'prepped_trend_model'))) {
    prepped_model <- attr(x$model_data, 'prepped_trend_model')
    other_vars <- c(
      other_vars,
      c(prepped_model$unit, prepped_model$gr, prepped_model$subgr)
    )
  }

  if (flatten) {
    other_vars <- setdiff(unique(other_vars), c('NA', preds))
    preds <- c(preds, other_vars)
  } else {
    other_vars <- setdiff(unique(other_vars), c('NA', preds$conditional))
    preds$conditional <- c(preds$conditional, other_vars)
  }

  return(preds)
}
