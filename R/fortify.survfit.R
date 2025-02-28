
#' @title Convert \link[survival]{survfit.object} to \link[base]{data.frame}
#' 
#' @description ..
#' 
#' @param model \link[survival]{survfit.object}.  
#' `'survfitcox'` object returned from 
#' `survival:::survfit.coxph` is also supported, as of 2022-09-19
#' 
#' @param ... additional parameters of \link[base]{as.data.frame.list}
#' 
#' @param times (optional) \link[base]{numeric} vector
#' 
#' @param units \link[base]{character}
#' 
#' @param oneminus \link[base]{logical} scalar
#' 
#' @param rm01 \link[base]{logical} scalar, whether to remove survival rates (and confidence bands) 
#' being 0 or 1, as some y-axis transformation may yield `NA` or `Inf` values.  Default `FALSE`.
#' 
#' @details 
#' A column named `'strata'`
#' of all-equal values `'all_subjects'` 
#' for \link[survival]{survfit.object} without a strata.
#' 
#' @returns 
#' Function [fortify.survfit] returns a \link[base]{data.frame}
#' 
#' @seealso 
#' `survival:::summary.survfit`
#' 
#' @importFrom utils glob2rx
#' @importFrom ggplot2 fortify
#' @export fortify.survfit
#' @export
fortify.survfit <- function(
    model, ..., 
    times,
    units = '',
    oneminus = FALSE,
    rm01 = FALSE
) {
  
  x <- model; model <- NULL
  
  if ((fom <- x$call$formula)[[1L]] == '~') { # has true formula
    if (is.symbol(fom[[3L]])) {
      ptn0 <- glob2rx(paste0(deparse1(fom[[3L]]), '='))
      ptn <- substr(ptn0, start = 2L, stop = nchar(ptn0) - 1L)
      names(x$strata) <- gsub(pattern = ptn, replacement = '', x = names(x$strata))
    }
  }
  
  # ?survival:::summary.survfit
  # `censored`, ignored if !missing(times); should the censoring times be included in the output?
  # `extend`, ignored if missing(times); even if there are no subjects left at the end of the specified times
  
  if (!missing(times)) {
    
    ret <- summary(x, times = times, extend = TRUE, data.frame = TRUE)
    if (oneminus) ret <- oneminus.summary.survfit(ret)
    ret$txt <- sprintf(
      #fmt = '%.1f%% (95%% CI %.1f%%~%.1f%%) at t=%.4g %s', 
      fmt = '%.1f%% (%.1f%%, %.1f%%) @ t=%.4g', 
      1e2 * ret$surv, 
      1e2 * ret$lower, 
      1e2 * ret$upper, 
      ret$time#, 
      #units %||% ''
    )
    
  } else {
    
    d1 <- summary(x, times = 0, data.frame = TRUE)
    d2 <- summary(x, censored = TRUE, data.frame = TRUE)
    if (oneminus) {
      d1 <- oneminus.summary.survfit(d1)
      d2 <- oneminus.summary.survfit(d2)
    }
    ret <- rbind.data.frame(d1, d2)
    
  }
  
  if (rm01) {
    id <- (ret$time > 0) & (ret$surv < 1) & (ret$surv > 0) & (ret$upper < 1) & (ret$upper > 0) & (ret$lower < 1) & (ret$lower > 0)
    ret <- ret[id, ]
  } # else do_nothing
  
  return(ret)
}


oneminus.survfit <- function(x) {
  .Defunct(msg = 'do not use this!  survival:::summary.survfit(oneminus.survfit(.)) is unpredictable')
  x$surv <- 1 - x$surv
  x$lower <- 1 - x$lower
  x$upper <- 1 - x$upper
  # x$cumhaz <- -x$cumhaz # ???
  return(x)
}

oneminus.summary.survfit <- function(x) {
  x$surv <- 1 - x$surv
  x$lower <- 1 - x$lower
  x$upper <- 1 - x$upper
  # x$cumhaz <- -x$cumhaz # ???
  return(x)
}


