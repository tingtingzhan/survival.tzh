

#' @title Convert \link[survival]{quantile.survfit} to \link[flextable]{flextable}
#' 
#' @description ..
#' 
#' @param x a \link[survival]{survfit.object}
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
  
  nstrata <- length(x$strata)

  q. <- x |>
    quantile(...) |>
  # ?survival:::quantile.survfit # 2026-01-09
  # it seems the confidence level (95%) is hard coded
    sprintf_quantile_survfit(fmt = fmt)
  
  z <- q.[['quantile']]
  z[] <- sprintf(fmt = '%s (%s, %s)', q.[['quantile']], q.[['lower']], q.[['upper']])
  
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
    as_flextable.matrix(row.title = deparse1(x$call$formula[[2L]])) |> 
    color(i = seq_len(nstrata), color = if (nstrata) pal_hue()(n = nstrata), part = 'body') |>
    set_caption(caption = table_caption)
  
}




sprintf_quantile_survfit <- \(fmt, x, ...) {
  
  # `x` is the return of ?survival:::quantile.survfit
  
  z <- x
  
  q. <- x[['quantile']] 
  q.[!is.na(q.)] <- q.[!is.na(q.)] |>
    sprintf(fmt = fmt)
  q.[is.na(q.)] <- '> max.fu'
  if (is.matrix(q.)) {
    colnames(q.) <- colnames(q.) |>
      sprintf(fmt = '%s%% Observed') # ?survival:::quantile.survfit does not take care of this ..
  } else {
    names(q.) <- names(q.) |>
      sprintf(fmt = '%s%% Observed') # ?survival:::quantile.survfit does not take care of this ..
  }
  z[['quantile']] <- q.
  
  l <- x[['lower']]
  l[is.na(l)] <- 0 # unestimable LCL is 0
  l[] <- l |> 
    sprintf(fmt = fmt)
  z[['lower']] <- l
  
  u <- x[['upper']]
  u[!is.na(u)] <- u[!is.na(u)] |>
    sprintf(fmt = fmt)
  u[is.na(u)] <- '\u221e' # unestimable UCL is Inf
  z[['upper']] <- u
  
  return(z)
  
}

