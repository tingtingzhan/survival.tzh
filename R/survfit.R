

#' @title Time Unit of a \link[survival]{survfit.object} or \link[survival]{summary.survfit} Object
#' 
#' @description ..
#' 
#' @param x a \link[survival]{survfit.object} or \link[survival]{summary.survfit} object
#' 
#' @seealso 
#' \link[base]{units}
#' 
#' @keywords internal
#' @name units_survfit
#' @export units.survfit
#' @export
units.survfit <- function(x) {
  #attr(x, which = 'units', exact = TRUE) %||% # brutal-but-simple for downstream packages!!
  attr(x[['time']], which = 'units', exact = TRUE) %||%
    tryCatch(units.Surv(eval(x$call$data)[[x$call$formula[[2L]]]]), error = \(e) return(invisible()))
}
  
#' @rdname units_survfit
#' @method units summary.survfit
#' @export units.summary.survfit
#' @export
units.summary.survfit <- units.survfit

  

#' @rdname more_units_set
#' @returns 
#' The `S3` method [more_units<-.survfit] returns a \link[survival]{survfit.object}.
#' @export more_units<-.survfit
#' @export
`more_units<-.survfit` <- function(x, value) {
  x[['time']] <- x[['time']] |> # |> is.vector(mode = 'double')
    .difftime(units = value)
  return(x)
}


#' @rdname more_units_set
#' @returns 
#' The `S3` method [more_units<-.summary.survfit] returns a \link[survival]{summary.survfit} object.
#' @method more_units<- summary.survfit
#' @export more_units<-.summary.survfit
#' @export
`more_units<-.summary.survfit` <- `more_units<-.survfit`


  
  
#' @title Layers of Kaplan-Meier Curve of \link[survival]{survfit.object} using \CRANpkg{ggplot2}
#' 
#' @description ..
#' 
#' @param object a \link[survival]{survfit.object}
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
  ss <- object |> 
    # summary(censored = TRUE) |> # 
    summary( # ?survival:::summary.survfit
      times = c(0, object$time) |> 
        unique.default() # more robust!!
    ) 
  #attr(ss, which = 'units') <- attr(object, which = 'units', exact = TRUE) # have to go the silly way..
  more_units(ss) <- units.survfit(object) # `more_units<-.summary.survfit`
  ss |> 
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
#' @importFrom ggplot2 autolayer aes geom_ribbon geom_step scale_fill_discrete geom_point scale_colour_discrete labs
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
      x = units.summary.survfit(object) %||% 'Time',
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




#' @title Sample Size of \link[survival]{survfit.object}
#' 
#' @param x a \link[survival]{survfit.object} or \link[survival]{summary.survfit} object
#' 
#' @examples
#' library(ecip)
#' survfit(Surv(time, status) ~ x, data = aml) |> 
#'  nobsText()
#' survfit(Surv(time, status) ~ x, data = aml) |> 
#'  summary() |>
#'  nobsText()
#' @keywords internal
#' @importFrom ecip nobsText
#' @name nobsText_survfit
#' @export nobsText.survfit
#' @export
nobsText.survfit <- function(x) {
  sprintf(fmt = '%d subj (%d events)', sum(x[['n']]), sum(x[['n.event']]))
}

#' @rdname nobsText_survfit
#' @method nobsText summary.survfit
#' @export nobsText.summary.survfit
#' @export
nobsText.summary.survfit <- nobsText.survfit











#' @title Fast Markdown Lines for \link[survival]{survfit.object}
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
#' @importClassesFrom fastmd md_lines  
#' @importFrom fastmd md_
#' @export md_.survfit
#' @export
md_.survfit <- function(x, xnm, ...) {
  
  z1 <- x$call$formula[[2L]] |> 
    deparse1() |> 
    sprintf(fmt = '@KaplanMeier58 estimates and curves of the time-to-event endpoint **`%s`** are obtained using <u>**`R`**</u> package <u>**`survival`**</u>.') |>
    new(Class = 'md_lines', package = 'survival', bibentry = .kaplan_meier58())
  
  z2 <- c(
    '```{r}',
    '#| echo: false', 
    '#| comment: ',
    xnm |> 
      sprintf(fmt = 'as_flextable_quantile_survfit(%s)'),
    '```'
  ) |> 
    new(Class = 'md_lines')
  
  z3 <- c(
    '```{r}',
    '#| echo: false', 
    '#| warning: false', 
    '#| dev: \'ragg_png\'', # unicode support for \pkg{rpart.tzh}
    x |>
      attr(which = 'fig-height', exact = TRUE) |> 
      sprintf(fmt = '#| fig-height: %.1f'),
    x |>
      attr(which = 'fig-width', exact = TRUE) |> 
      sprintf(fmt = '#| fig-width: %.1f'),
    
    #xnm |> sprintf(fmt = 'autoplot.survfit(%s)'),
    xnm |> sprintf(fmt = 'autoplot(%s)'),
    # use generic ?ggplot2::autoplot; May dispatch to
    # ?survival.tzh::autoplot.survfit
    # ?rpart.tzh::autoplot.rpart
    '```'
  ) |> 
    new(Class = 'md_lines')
  
  c(z1, z2, z3) # ?fastmd::c.md_lines
  
}




#' @title Fast Markdown Lines for \link[survival]{summary.survfit} Object
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
#' @importClassesFrom fastmd md_lines  
#' @importFrom fastmd md_
#' @export md_.summary.survfit
#' @export
md_.summary.survfit <- function(x, xnm, ...) {
  
  c(
    '```{r}',
    '#| echo: false', 
    xnm |> 
      sprintf(fmt = '%s |> as_flextable.summary.survfit(which = \'surv\')'), 
    '```'
  ) |> 
    new(Class = 'md_lines')
  
}







#' @title Convert \link[survival]{summary.survfit} Object to \link[flextable]{flextable}
#' 
#' @param x a \link[survival]{summary.survfit} object
#' 
#' @param which ..
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom flextable as_flextable color set_caption
#' @importFrom fastmd as_flextable.matrix
#' @importFrom reshape2 acast dcast
#' @importFrom scales pal_hue
#' @export as_flextable.summary.survfit
#' @export
as_flextable.summary.survfit <- function(
    x, 
    which = c('n.risk', 'surv'), 
    ...
) {
  
  which <- match.arg(which)
  
  x0 <- x # original `x`, just in case
  
  # add confint to `$surv`
  x$surv <- sprintf(
    fmt = '%.1f%%\n(%.1f%%, %.1f%%)', 
    1e2*x$surv, 1e2*x$lower, 1e2*x$upper
  )
  
  nstrata <- x$strata |> 
    levels() |> 
    length()
  z <- if (nstrata) {# with strata
    x[c('strata', which, 'time')] |>
      as.data.frame.list() |>
      acast(formula = strata ~ time, value.var = which)
  } else {# without strata
    x[c(which, 'time')] |>
      as.data.frame.list() |> 
      acast(formula = . ~ time, value.var = which)
  }
  
  unt <- units.summary.survfit(x) %||% 'Time'
  names(dimnames(z)) <- c('Strata', unt)
  
  table_title <- switch(which, n.risk = {
    'Number at Risk'
  }, surv = {
    sprintf(fmt = 'Percentage Survived (%.0f%% Confidence Interval)', 1e2*x$conf.int)
  })
  
  z |> 
    as_flextable.matrix() |>
    color(i = seq_len(nstrata), color = if (nstrata) pal_hue()(n = nstrata), part = 'body') |>
    set_caption(caption = table_title)
  
}


