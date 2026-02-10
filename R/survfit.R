
#' @title Layers of Kaplan-Meier Curve of \link[survival]{survfit.object} using \CRANpkg{ggplot2}
#' 
#' @description ..
#' 
#' @param object \link[survival]{survfit.object}
#' 
# @param ribbon \link[base]{logical} scalar, default `TRUE`
#' 
# @param labels (optional) \link[base]{character} \link[base]{vector}
#' 
# @param oneminus \link[base]{logical} scalar
#' 
#' @param ... additional parameters of function [autolayer.summary.survfit()]
#' 
#' @note
#' It's very difficulty to grab the time \link[base]{units} of the response of a 
#' \link[survival]{survfit} or \link[survival]{summary.survfit} object.
#' The only way is `eval(object$call$formula[[2L]], envir = eval(object$call$data))`,
#' which is very vulnerable as `object$call$data` might be defined in some other internal functions.
#' Therefore we leave the specification of `xlab` (ideally the time unit) to the end user.
#' 
#' @keywords internal
#' @importFrom ggplot2 autolayer
#' @export autolayer.survfit
#' @export
autolayer.survfit <- function(object, ...) {
  # see ?survival:::summary.survfit
  object |> 
    # summary(censored = TRUE) |> # 
    summary(times = c(0, object$time) |> unique.default()) |> # more robust!!
    autolayer.summary.survfit(...)
}




#' @title Layers of Kaplan-Meier Curve of \link[survival]{summary.survfit} using \CRANpkg{ggplot2}
#' 
#' @description ..
#' 
#' @param object \link[survival]{summary.survfit}
#' 
#' @param ribbon \link[base]{logical} scalar, default `TRUE`
#' 
#' @param labels (optional) \link[base]{character} \link[base]{vector}
#' 
#' @param oneminus \link[base]{logical} scalar
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom ggplot2 autolayer aes geom_ribbon geom_step scale_fill_discrete geom_point scale_colour_discrete
#' @importFrom stats setNames
#' @export autolayer.summary.survfit
#' @export
autolayer.summary.survfit <- function(
    object, 
    ribbon = TRUE,
    labels = NULL,
    oneminus = FALSE,
    ...
) {
  
  # how do I ensure `survival:::summary.survfit(., censored = TRUE)` ??
  
  fom <- object$call$formula
  if (fom[[1L]] != '~') stop('must preserve the true `formula`')
  
  if (oneminus) {
    object <- object |> 
      oneminus.summary.survfit()
  }
  
  labels <- object$strata |>
    attr(which = 'levels', exact = TRUE)
  # NULL compatible!
  
  id_c <- (object$n.censor > 0L)
  
  strata_nm <- if (is.symbol(fom[[3L]])) deparse1(fom[[3L]]) # else NULL
  
  return(list( 
    
    geom_step(
      mapping = aes(x = object$time, y = object$surv, group = object$strata, colour = object$strata),
    ), 
    
    if (ribbon) geom_ribbon(
      mapping = aes(x = object$time, ymax = object$upper, ymin = object$lower, group = object$strata, fill = object$strata), 
      alpha = .1),
    
    geom_point(
      mapping = aes(x = object$time[id_c], y = object$surv[id_c], group = object$strata[id_c], colour = object$strata[id_c]),
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
#' @export autoplot.survfit
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










#' @title md_.survfit
#' 
#' @description ..
#' 
#' @param x \link[survival]{survfit.object}
#' 
#' @param xnm ..
#'  
#' @param ... ..
#' 
#' @examples
#' s0 = survfit(Surv(time, status) ~ 1, data = aml)
#' s1 = survfit(Surv(time, status) ~ x, data = aml)
#' list(
#'  'no strata' = list(s0, summary(s0, times = c(10, 25))),
#'  'one strata' = list(s1, summary(s1, times = c(10, 25)))
#' ) |> fastmd::render_(file = 'survfit')
#' @keywords internal
#' @importFrom methods new
#' @importClassesFrom fastmd md_lines  
#' @importFrom fastmd md_
#' @export md_.survfit
#' @export
md_.survfit <- function(x, xnm, ...) {
  
  z1 <- x$call$formula[[2L]] |> 
    deparse1() |> 
    sprintf(fmt = '@KaplanMeier58 estimates and curves of the time-to-event endpoint **`%s`** are obtained using <u>**`R`**</u> package <u>**`survival`**</u>.') |>
    new(Class = 'md_lines', package = 'survival', bibentry = KaplanMeier58())
  
  z2 <- c(
    '<details><summary>Median Survival (& Confidence Interval)</summary>',
    '```{r}',
    '#| echo: false', 
    '#| comment: ',
    # xnm,
    xnm |> sprintf(fmt = 'tmp <- %s'), 
    'tmp$call <- NULL', # see ?survival:::print.survfit
    'tmp',
    '```',
    '</details>'
  ) |> 
    new(Class = 'md_lines')
  
  z3 <- c(
    '```{r}',
    '#| echo: false', 
    '#| warning: false', 
    x |>
      attr(which = 'fig-height', exact = TRUE) |> 
      sprintf(fmt = '#| fig-height: %.1f'),
    x |>
      attr(which = 'fig-width', exact = TRUE) |> 
      sprintf(fmt = '#| fig-width: %.1f'),
    
    xnm |> sprintf(fmt = 'autoplot.survfit(%s)'),
    '```'
  ) |> 
    new(Class = 'md_lines')
  
  c(z1, z2, z3) # ?fastmd::c.md_lines
  
}




#' @title [md_.summary.survfit]
#' 
#' @description ..
#' 
#' @param x a \link[survival]{summary.survfit} object
#' 
#' @param xnm ..
#'  
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom methods new
#' @importClassesFrom fastmd md_lines  
#' @importFrom fastmd md_
#' @export md_.summary.survfit
#' @export
md_.summary.survfit <- function(x, xnm, ...) {
  
  c(
    '```{r}',
    '#| echo: false', 
    xnm |> sprintf(fmt = '%s |> as_flextable.summary.survfit(which = \'surv\')'), 
    '```'
  ) |> 
    new(Class = 'md_lines')
  
}


