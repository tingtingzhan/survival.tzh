
#' @title Layers of Kaplan-Meier Curve of \link[survival]{survfit.object} using \CRANpkg{ggplot2}
#' 
#' @description ..
#' 
#' @param object \link[survival]{survfit.object}
#' 
#' @param ribbon \link[base]{logical} scalar, default `TRUE`
#' 
#' @param labels (optional) \link[base]{character} \link[base]{vector}
#' 
#' @param oneminus \link[base]{logical} scalar
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
#' @keywords internal
#' @importFrom ggplot2 autolayer aes geom_ribbon geom_step scale_fill_discrete geom_point scale_colour_discrete
#' @importFrom stats setNames
#' @export autolayer.survfit
#' @export
autolayer.survfit <- function(
    object, 
    ribbon = TRUE,
    labels = NULL,
    oneminus = FALSE,
    ...
) {
  
  fom <- object$call$formula
  if (fom[[1L]] != '~') stop('must preserve the true `formula`')
  
  if (!length(object$strata)) object$strata <- setNames(length(object$time), nm = 'all_subjects')
  
  d <- object |> 
    summary(censored = TRUE) # ?survival:::summary.survfit
  # `censored`, ignored if !missing(times); should the censoring times be included in the output?
  # `extend`, ignored if missing(times); even if there are no subjects left at the end of the specified times
  if (oneminus) {
    d <- d |> 
      oneminus.summary.survfit()
  }
  
  old_labs <- d$strata |>
    attr(which = 'levels', exact = TRUE)
  
  if (missing(labels)) {
    labels <- old_labs
    
  } else if (is.character(labels)) {
    if (!length(labels) || anyNA(labels) || !all(nzchar(labels))) stop('illegal labels')
    if (length(labels) != length(names(object$strata))) stop('user specified `labels` not match the strata of `survfit` object')
    if (!is.null(names(labels))) stop('user specified `labels` must be unnamed!  Watch the order of default saput first, then write user-specified labels')
    sprintf(fmt = '%s was %s\n', sQuote(labels), sQuote(old_labs)) |> message()
    
  } else if (isFALSE(labels) || !length(labels)) {
    labels <- NULL
    
  } else stop('illegal `labels`')
  
  id_c <- (d$n.censor > 0L)
  
  strata_nm <- if (is.symbol(fom[[3L]])) deparse1(fom[[3L]]) # else NULL
  
  return(list( 
    
    geom_step(
      mapping = aes(x = d$time, y = d$surv, group = d$strata, colour = d$strata),
    ), 
    
    if (ribbon) geom_ribbon(
      mapping = aes(x = d$time, ymax = d$upper, ymin = d$lower, group = d$strata, fill = d$strata), 
      alpha = .1),
    
    geom_point(
      mapping = aes(x = d$time[id_c], y = d$surv[id_c], group = d$strata[id_c], colour = d$strata[id_c]),
      shape = 3L),
    
    (if (length(labels)) scale_colour_discrete(labels = labels)),
    
    (if (length(labels)) scale_fill_discrete(labels = labels)),
    
    labs(
      x = tryCatch(units.Surv(eval(object$call$data)[[fom[[2L]]]]), error = \(e) 'Time'),
      y = deparse1(fom[[2L]]), 
      colour = strata_nm, 
      fill = strata_nm
    )
    
  ))

}





#' @title Kaplan-Meier Curve of \link[survival]{survfit.object} using \CRANpkg{ggplot2}
#' 
#' @description ..
#' 
#' @param object \link[survival]{survfit.object}
#' 
#' @param ... additional parameters of function [autolayer.survfit()]
#' 
#' @seealso 
#' `survival:::plot.survfit` `survival:::quantile.survfit`
#' 
#' @importFrom ggplot2 autoplot ggplot scale_y_continuous
#' @importFrom scales label_percent
#' @export
autoplot.survfit <- function(object, ...) {
  ggplot() + 
    autolayer.survfit(object, ...) + 
    scale_y_continuous(labels = label_percent())
}




#' @title S3 Methods for \link[survival]{survfit} Objects
#' 
#' @param x \link[survival]{survfit} or \link[survival]{summary.survfit} object
#' 
#' @examples
#' survfit(Surv(time, status) ~ x, data = aml) |> 
#'  nobsText.survfit()
#' @keywords internal
#' @importFrom ecip nobsText
#' @export nobsText.survfit
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
#' survfit(Surv(time, status) ~ 1, data = aml) |> Sprintf.survfit()
#' survfit(Surv(time, status) ~ x, data = aml) |> Sprintf.survfit()
#' @keywords internal
#' @importFrom methods new
#' @importClassesFrom rmd.tzh md_lines  
#' @importFrom ecip Sprintf
#' @export Sprintf.survfit
#' @export
Sprintf.survfit <- function(x) {
  # read ?survival::survfit.formula carefully
  # how to tell 'single event' or not ?
  fom <- x$call$formula
  edp <- deparse1(fom[[2L]])
  fmt <- '@KaplanMeier58 estimates and curves of time-to-event endpoint **`%s`** are obtained using <u>**`R`**</u> package <u>**`survival`**</u>'
  ret <- if (identical(fom[[3L]], 1)) {
    # no predictor
    sprintf(fmt = paste0(fmt, '.'), edp)
  } else {
    sprintf(fmt = paste0(fmt, ', by predictor(s) %s.'),
            edp,
            paste0('`', all.vars(fom[[3L]]), '`', collapse = ','))
  }
  
  ret |>
    new(Class = 'md_lines', package = 'survival', bibentry = KaplanMeier58())
  
}







