
#' @title Layers of Kaplan-Meier Curve of \link[survival]{survfit.object} using \CRANpkg{ggplot2}
#' 
#' @description ..
#' 
#' @param object \link[survival]{survfit.object}
#' 
#' @param times (optional) \link[base]{numeric} scalar or \link[base]{vector},
#' time points where survival rates as well as the confidence intervals are plotted
#' 
#' @param ribbon \link[base]{logical} scalar, default `TRUE`
#' 
#' @param labels (optional) \link[base]{character} \link[base]{vector}
#' 
#' @param ... ..
#' 
#' @note
#' It's very difficulty to grab the time \link[base]{units} of the response of a 
#' \link[survival]{survfit} object.
#' The only way is `eval(object$call$formula[[2L]], envir = eval(object$call$data))`,
#' which is very vulnerable as `object$call$data` might be defined in some other internal functions.
#' Therefore we leave the specification of `xlab` (ideally the time unit) to the end user.
#' 
#' @importFrom ggplot2 autolayer aes geom_ribbon geom_step scale_fill_discrete geom_point scale_colour_discrete
#' @importFrom ggrepel geom_label_repel
#' @importFrom stats setNames aggregate.data.frame quantile
#' @importFrom survival survdiff
#' @export autolayer.survfit
#' @export
autolayer.survfit <- function(
    object, 
    times,
    ribbon = TRUE,
    labels = NULL,
    ...
) {
  
  fom <- object$call$formula
  if (fom[[1L]] != '~') stop('must preserve the true `formula`')
  
  if (!length(object$strata)) object$strata <- setNames(length(object$time), nm = 'all_subjects')
  
  d <- fortify.survfit(object, ...)
  old_labs <- attr(d$strata, which = 'levels', exact = TRUE)
  
  if (missing(labels)) {
    ns <- aggregate.data.frame(d[c('n.event', 'n.censor')], by = d['strata'], FUN = sum)
    sm <- quantile(object, probs = .5, conf.int = TRUE) # survival:::quantile.survfit
    #obj_sum <- summary(object)
    #maxFU <- vapply(split.default(obj_sum$time, f = obj_sum$strata), FUN = max, FUN.VALUE = 0)
    # I have difficulty getting the censor time from 'survfit' object
    maxFU <- max(object$time)
    sm_txt <- ifelse(
      test = !is.na(c(sm$quantile)), 
      yes = sprintf(fmt = 't50 = %.1f (%.1f, %.1f)', c(sm$quantile), c(sm$lower), c(sm$upper)),
      no = sprintf(fmt = 't50 > %.1f', maxFU))
    labels <- sprintf(fmt = '%s\n%d events; %d censors\n%s', 
                      old_labs, ns[['n.event']], ns[['n.censor']], sm_txt)
    
  } else if (is.character(labels)) {
    if (!length(labels) || anyNA(labels) || !all(nzchar(labels))) stop('illegal labels')
    if (length(labels) != length(names(object$strata))) stop('user specified `labels` not match the strata of `survfit` object')
    if (!is.null(names(labels))) stop('user specified `labels` must be unnamed!  Watch the order of default saput first, then write user-specified labels')
    message(sprintf(fmt = '%s was %s\n', sQuote(labels), sQuote(old_labs)))
    
  } else if (isFALSE(labels) || !length(labels)) {
    labels <- NULL
    
  } else stop('illegal `labels`')
  
  d_c <- d[d$n.censor > 0L, , drop = FALSE]
  
  strata_nm <- if (is.symbol(fom[[3L]])) deparse1(fom[[3L]]) # else NULL
  
  mp_step <- call(name = 'aes', x = quote(time), y = quote(surv), group = quote(strata), colour = quote(strata))
    
  ret <- list( 
    
    geom_step(data = d, mapping = eval(mp_step), 
              alpha = if (!missing(times)) .5 else 1), 
    
    if (ribbon) geom_ribbon(data = d, mapping = eval(call(
      name = 'aes', 
      x = quote(time), 
      ymax = quote(upper), ymin = quote(lower), 
      group = quote(strata), fill = quote(strata)
    )), alpha = .1),
    
    geom_point(data = d_c, mapping = eval(mp_step), shape = 3L,
               alpha = if (!missing(times)) .5 else 1),
    
    if (!missing(times)) {
      yr <- fortify.survfit(object, times = times, ...)
      geom_label_repel(data = yr, mapping = eval(call(
        name = 'aes', 
        x = quote(time), 
        y = quote(surv), 
        colour = quote(strata), #fill = quote(strata), 
        label = quote(txt)
      )), fontface = 'bold', fill = 'transparent',
      size = 3)
    },
    (if (length(labels)) scale_colour_discrete(labels = labels)),
    (if (length(labels)) scale_fill_discrete(labels = labels)),
    labs(y = deparse1(fom[[2L]]), colour = strata_nm, fill = strata_nm)
  )

  attr(ret, which = 'data') <- d
  return(ret)
  
}





#' @title Kaplan-Meier Curve of \link[survival]{survfit.object} using \CRANpkg{ggplot2}
#' 
#' @description ..
#' 
#' @param object \link[survival]{survfit.object}
#' 
#' @param ... additional parameters of function [autolayer.survfit]
#' 
#' @seealso 
#' `survival:::plot.survfit` `survival:::quantile.survfit`
#' 
#' @importFrom ggplot2 ggplot scale_y_continuous
#' @importFrom scales percent
#' @export
autoplot.survfit <- function(object, ...) {
  ggplot() + 
    autolayer.survfit(object, ...) + 
    scale_y_continuous(labels = percent)
}




#' @title S3 Methods for \link[survival]{survfit} Objects
#' 
#' @param x \link[survival]{survfit} or \link[survival]{summary.survfit} object
#' 
#' @examples
#' m = survfit(Surv(time, status) ~ x, data = aml)
#' m |> conf_level.survfit()
#' m |> summary() |> conf_level.summary.survfit()
#' m |> nobsText.survfit()
#' 
#' @name S3_survfit
#' @export
conf_level.survfit <- function(x) x[['conf.int']]

#' @rdname S3_survfit
#' @export
conf_level.summary.survfit <- conf_level.survfit

#' @rdname S3_survfit
#' @export
nobsText.survfit <- function(x) {
  sprintf(fmt = '%d subj (%d events)', sum(x[['n']]), sum(x[['n.event']]))
}










#' @title Sprintf.survfit
#' 
#' @description ..
#' 
#' @param x \link[survival]{survfit.object}
#' 
#' @examples
#' Sprintf.survfit(survfit(Surv(time, status) ~ 1, data = aml))
#' Sprintf.survfit(survfit(Surv(time, status) ~ x, data = aml))
#' @export Sprintf.survfit
#' @export
Sprintf.survfit <- function(x) {
  # read ?survival::survfit.formula carefully
  # how to tell 'single event' or not ?
  fom <- x$call$formula
  edp <- deparse1(fom[[2L]])
  fmt <- 'Kaplan-Meier estimates and curves of time-to-event endpoint **`%s`** are obtained using <u>**`R`**</u> package <u>**`survival`**</u>'
  if (identical(fom[[3L]], 1)) {
    # no predictor
    sprintf(fmt = paste0(fmt, '.'), edp)
  } else {
    sprintf(fmt = paste0(fmt, ', by predictor(s) %s.'),
            edp,
            paste0('`', all.vars(fom[[3L]]), '`', collapse = ','))
  }
}







