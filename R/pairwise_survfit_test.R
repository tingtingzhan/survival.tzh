

#' @title pairwise_survfit_test
#' 
#' @description ..
#' 
#' @param x \link[survival]{survfit.object}
#' 
#' @param times \link[base]{numeric} scalar or \link[base]{vector}, see parameter `times` of 
#' `survival:::summary.survfit`
#' 
#' @param units \link[base]{character} scalar, the time unit
#' 
#' @param ... ..
#' 
#' @details ..
#' 
#' @examples 
#' library(survival)
#' x = survfit(Surv(stop-start, event = (status!=0)) ~ treatment, 
#'   data = bladder1)
#' pairwise_survfit_test(x, times = c(10, 20))
#' 
#' @seealso 
#' \link[stats]{pairwise.table}
#' 
#' @export
pairwise_survfit_test <- function(
    x, 
    times = stop('must provide time point'), 
    units = attr(x, which = 'units', exact = TRUE), 
    ...
) {
  if (!inherits(x, what = 'survfit')) stop('must have survfit input')
  if (!is.numeric(times) || !length(times) || anyNA(times) || any(times < 0)) stop('illegal times')
  times <- unique.default(times)
  xsum <- summary(x, times = times, extend = TRUE, ...)
  if (is.null(strata <- xsum$strata)) return(invisible()) # exception  #stop('must have strata')
  
  if (!xsum$logse) stop('wont be able to find std.eff, see survival:::summary.survfit')
  if (is.null(xsum$std.err)) stop('wont be able to find std.eff, see survival:::summary.survfit')
  
  if (length(times) == 1L) {
    return(pairwise_survfit_int(xsum$surv, std.err = xsum$std.err, strata = strata, data.name = paste('At time', times, units), ...))
  }
  
  ids <- split.default(seq_along(xsum$time), f = as.factor(xsum$time))
  
  ret <- .mapply(FUN = \(id, time) {
    pairwise_survfit_int(surv = xsum$surv[id], std.err = xsum$std.err[id], strata = strata[id], data.name = paste('At time', time, units), ...)
  }, dots = list(id = ids, time = times), MoreArgs = NULL)
  names(ret) <- names(ids)
  return(ret)
  
}



#' @importFrom stats p.adjust pchisq qlogis
pairwise_survfit_int <- function(
  surv,
  std.err,
  strata,
  data.name = '',
  trans = c('log', 'loglog', 'arcsin_sqrt', 'log_arcsin_sqrt', 'logit'), 
  p.adjust.method = 'none',
  ...
) {
  
  # \doi{10.1002/sim.2864}
  
  sqrtV <- std.err
  std.err <- sqrtV / surv # sigma
  n <- length(strata)
  
  log_s <- log(surv)
  loglog_s <- log(-log(surv))
  selog_s <- std.err / log_s
  nu <- surv * std.err^2 / (4*(1 - surv))
  asinsq_s <- asin(sqrt(surv))
  mu <- nu / asinsq_s^2
  logit_s <- qlogis(surv)
  seinv_s <- std.err / (1 - surv)
  
  o_ <- function(X, ...) outer(X, Y = X, ...)
  
  switch(trans <- match.arg(trans), log = {# Equation (3)
    chisq_q <- o_(log_s, FUN = `-`)^2 / o_(std.err^2, FUN = `+`)
    trans_txt <- 'Log'
  }, loglog = { # Equation (4)
    chisq_q <- o_(loglog_s, FUN = `-`)^2 / o_(selog_s^2, FUN = `+`)
    trans_txt <- 'Log-Log'
  }, arcsin_sqrt = { # Equation (5)
    chisq_q <- o_(asinsq_s, FUN = `-`)^2 / o_(nu, FUN = `+`)
    trans_txt <- 'ArcSine-Squared-Root'
  }, log_arcsin_sqrt = { # Equation (6)
    chisq_q <- o_(log(asinsq_s), FUN = `-`)^2 / o_(mu, FUN = `+`)
    trans_txt <- 'Log-ArcSine-Squared-Root'
  }, logit = { # Equation (7)
    chisq_q <- o_(logit_s, FUN = `-`)^2 / o_(seinv_s^2, FUN = `+`)
    trans_txt <- 'Logit'
  }, stop('illegal trans: ', sQuote(trans)))
  
  # see \link[stats]{pairwise.table} carefully
  dimnames(chisq_q) <- list(str <- as.character(strata), str)
  chisq_stat <- chisq_q[-1L, -n]
  chisq_stat[!lower.tri(chisq_stat, diag = TRUE)] <- NA_real_
  pval <- pchisq(q = chisq_stat, df = 1L, lower.tail = FALSE) # preserves 'matrix'
  pval[] <- p.adjust(pval, method = p.adjust.method)
  
  out <- list(
    p.value = pval,
    statistic = chisq_stat,
    parameter = c(df = 1L),
    data.name = data.name, 
    trans = trans,
    method = paste0('Chi-Squared Test of Survival Rates at Fixed Timepoint (', trans_txt, '-transformed)')
  )
  class(out) <- 'pairwise.htest'
  return(out)
}


