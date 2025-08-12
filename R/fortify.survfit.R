
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
#' @details 
#' A column named `'strata'`
#' of all-equal values `'all_subjects'` 
#' for \link[survival]{survfit.object} without a strata.
#' 
#' @returns 
#' Function [fortify.survfit()] returns a `survival:::summary.survfit` object.
#' 
#' @keywords internal
#' @importFrom utils glob2rx
#' @importFrom ggplot2 fortify
#' @export fortify.survfit
#' @export
fortify.survfit <- function(
    model, ..., 
    times,
    units = '',
    oneminus = FALSE
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
    
    ret <- summary(x, times = times, extend = TRUE)
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
    
    ret <- summary(x, censored = TRUE)
    if (oneminus) {
      ret <- oneminus.summary.survfit(ret)
    }

  }
  
  return(ret)
}






#' @title One-Minus Kaplan-Meier Estimates
#' 
#' @param x ..
#' 
#' @name oneminus
#' @export
oneminus.survfit <- function(x) {
  .Defunct(msg = 'do not use this!  survival:::summary.survfit(oneminus.survfit(.)) is unpredictable')
  x$surv <- 1 - x$surv
  x$lower <- 1 - x$lower
  x$upper <- 1 - x$upper
  # x$cumhaz <- -x$cumhaz # ???
  return(x)
}

#' @rdname oneminus
#' @export
oneminus.summary.survfit <- function(x) {
  
  # look carefully at 
  # survival:::summary.survfit(., data.frame = FALSE)
  # survival:::summary.survfit(., data.frame = TRUE)
  # we only care about the shared elements!!
  
  # names(x) # as of packageDate('survival') 2024-12-17
  
  # x$time: do **not** change
  # x$n.risk; x$n.event; x$n.censor: do **not** change
  
  x$surv <- 1 - x$surv # this is now 'percentage died'
  
  # x$std.err: do **not** change
  
  # x$cumhaz <- -x$cumhaz # what should tzh do?
  # plot(x$cumhaz + x$surv) # what does this mean???
  
  # x$std.chaz: do **not** change
  # x$strata: do **not** change
  
  x$lower <- 1 - x$lower
  x$upper <- 1 - x$upper
  
  return(x)
  
}


