

#' @title Convert \link[survival]{quantile.survfit} to \link[flextable]{flextable}
#' 
#' @description ..
#' 
#' @param x a \link[survival]{survfit} object
#' 
#' @param ... parameters of function \link[survival]{quantile.survfit}
#' 
#' @param fmt see function \link[base]{sprintf}
#' 
#' @param units (optional) \link[base]{character} scalar, unit of time in \code{x}
#' 
#' @note 
#' 
#' The confidence interval of median survival time is indeed defined by 
#' the intersection of confidence bands of Kaplan-Meier curve and the 50% survival horizontal line.
#' See **Details** of function \link[survival]{print.survfit}.
#' 
#' Function \link[survival]{quantile.survfit} returns a \link[base]{list}.
#' 
#' @seealso 
#' \link[survival]{quantile.survfit}, 
#' \link[survival]{median.survfit}
#' 
#' @examples 
#' survfit(Surv(time, status) ~ 1, data = aml) |>
#'  as_flextable_quantile_survfit(units = 'days') 
#' 
#' survfit(Surv(time, status) ~ x, data = aml) |>
#'  as_flextable_quantile_survfit(units = 'days') 
#' 
#' @keywords internal
#' @importFrom fastmd as_flextable.matrix
#' @importFrom flextable color
#' @importFrom scales pal_hue
#' @export
as_flextable_quantile_survfit <- function(
    x, ..., 
    fmt = '%.1f', 
    units = units.survfit(x)
) {
  
  if (!inherits(x, what = 'survfit')) stop('input must be survfit object')
  
  # max time, when median survival is not available
  nstrata <- length(x$strata)
  max_tm <- if (nstrata) {
    xs <- summary(x, censored = TRUE)
    xs$time |> 
      split.default(f = xs$strata) |>
      vapply(FUN = max, FUN.VALUE = 0) |> 
      tcrossprod(y = rep(1, length = nstrata))
  } else max(x$time)
  
  q. <- quantile(x, ...) 
  # ?survival:::quantile.survfit # 2026-01-09
  # it seems the confidence level (95%) is hard coded
  
  qq <- q.[['quantile']] 
  qq[!is.na(qq)] <- qq[!is.na(qq)] |>
    sprintf(fmt = fmt) # unestimable quantile is '>max_time'
  qq[is.na(qq)] <- max_tm[is.na(qq)] |>
    sprintf(fmt = paste0('>', fmt)) # unestimable quantile is '>max_time'
  
  l <- q.[['lower']]
  l[is.na(l)] <- 0 # unestimable LCL is 0
  
  u <- q.[['upper']]
  u[!is.na(u)] <- u[!is.na(u)] |>
    sprintf(fmt = fmt)
  u[is.na(u)] <- '\u221e' # unestimable UCL is Inf
  
  z <- q.[['quantile']]
  storage.mode(z) <- 'character'
  if (is.matrix(z)) {
    colnames(z) <- colnames(z) |>
      sprintf(fmt = '%s%% Observed') # ?survival:::quantile.survfit does not take care of this ..
  } else {
    names(z) <- names(z) |>
      sprintf(fmt = '%s%% Observed') # ?survival:::quantile.survfit does not take care of this ..
  }
  
  z[] <- sprintf(
    fmt = paste0('%s (', fmt, ', ', '%s)'),
    qq,
    l, u
  )
  
  if (!is.matrix(z)) {
    z <- z |>
      as.matrix.default() |>
      t.default()
  }
  
  table_caption <- (if (!length(units)) {
    'Time ('
  } else {
    units |> sprintf(fmt = 'Time (%s, ')
  }) |> 
    paste0(.x = _, '95% Confidence Interval) at Percentage Event-Observed')
  
  z |> 
    as_flextable.matrix(row.title = 'Strata') |> 
    color(i = seq_len(nstrata), color = if (nstrata) pal_hue()(n = nstrata), part = 'body') |>
    set_caption(caption = table_caption)
  
}
