


#' @title Kaplan Meier Curves of Multiple Endpoints
#' 
#' @description
#' Array of Kaplan-Meier curves of multiple endpoints and same predictors.
#' 
#' @param formula \link[stats]{formula}.  
#' E.g. `c(y1, y2) ~ x1 + x2` for
#' an array of Kaplan Meier curves of `y1 ~ x1 + x2` and `y2 ~ x1 + x2`
#' 
#' @param ... additional parameters of function \link[survival]{survfit.formula}
#' 
#' @returns
#' Function [msurvfit()] returns a `'survfitlist'`.
#' 
#' @keywords internal
#' @export
msurvfit <- function(formula, ...) {
  
  if (formula[[2L]][[1L]] != 'c') stop()
  
  s <- formula[[2L]][-1L] |> 
    as.list.default() |>
    lapply(FUN = \(edp) {
      do.call(what = survfit.formula, args = list(
        formula = eval(call(name = '~', edp, formula[[3L]])),
        ...
      ))
    })
  
  class(s) <- c('survfitlist', 'listof')
  return(s)

}
  





#' @title subset_survfit
#' 
#' @description
#' 
#' Multiple \link[survival]{survfit} in different subset.
#' 
#' @param formula \link[stats]{formula}
#' 
#' @param subset \link[base]{language} to indicate
#' the subset(s) of `data`
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param ... ..
#' 
#' @returns
#' Function [subset_survfit()] returns a `'survfitlist'`.
#' 
#' @keywords internal
#' @export
subset_survfit <- function(formula, subset, data, ...) {
  
  data.name <- substitute(data)
  
  e_ <- tryCatch(subset, error = identity)
  if (inherits(e_, what = 'error')) e_ <- substitute(subset)
  e <- e_ |> as.list.default()
  nm <- names(e[-1L])
  if (!length(nm) || anyNA(nm) || !all(nzchar(nm))) stop('`subset` must be fully named')
  
  d_call <- e[-1L] |> 
    lapply(FUN = \(e.) {
      if (isTRUE(e.)) return(data.name)
      call(name = 'subset.data.frame', x = data.name, subset = e.)
    })
  
  nr <- d_call |>
    vapply(FUN = \(d.) {
      eval(d.) |> 
        .row_names_info(type = 2L)
    }, FUN.VALUE = NA_integer_)
  
  s <- d_call |>
    lapply(FUN = \(d.) {
      do.call(what = survfit.formula, args = list(formula = formula, data = d., ...))
    })
  attr(s, which = 'title') <- sprintf(fmt = '%s, n=%d', nm, nr) # title (with sample size)
  class(s) <- c('survfitlist', 'listof')
  return(s)
  
}



#' @title autoplot.survfitlist
#' 
#' @param object ..
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom ggplot2 autoplot layer_scales 
#' @importFrom scales label_percent
#' @export autoplot.survfitlist
#' @export
autoplot.survfitlist <- function(object, ...) {
  
  p. <- object |>
    lapply(FUN = autoplot.survfit, ...)
  
  title. <- object |>
    attr(which = 'title', exact = TRUE)
  if (length(title.) == length(p.)) {
    p. <- mapply(FUN = \(p, title) {
      p + labs(title = title)
    }, p = p., title = title., SIMPLIFY = FALSE)
  }
  
  yl <- p. |>
    lapply(FUN = \(p) layer_scales(p)$y$range$range) |>
    range.default()
  
  p. |> 
    lapply(FUN = \(p) {
      suppressMessages(
        p + 
          scale_y_continuous(labels = label_percent(), limits = yl)
      )
    }) |>  
    Reduce(f = `+`, x = _) # 'patchwork'
  
}




#' @title [md_.survfitlist()]
#' 
#' @description ..
#' 
#' @param x a `'survfitlist'` object
#' 
#' @param xnm ..
#'  
#' @param ... ..
#' 
#' @keywords internal
#' @importClassesFrom fastmd md_lines  
#' @importFrom fastmd md_ md_autoplot_
#' @export md_.survfitlist
#' @export
md_.survfitlist <- function(x, xnm, ...) {
  
  z1 <- x |> 
    vapply(FUN = \(i) {
      i$call$formula[[2L]] |>
        deparse1()
    }, FUN.VALUE = '') |>
    unique.default() |>
    sprintf(fmt = '**`%s`**') |>
    paste(collapse = ', ') |>
    sprintf(fmt = '@KaplanMeier58 estimates and curves of time-to-event endpoint(s) %s are obtained using <u>**`R`**</u> package <u>**`survival`**</u>.') |>
    new(Class = 'md_lines', package = 'survival', bibentry = .kaplan_meier58())
  
  z2 <- xnm |> 
    sprintf(fmt = 'as_flextable_quantile_survfitlist(%s)') |> 
    new(Class = 'md_lines', chunk.r = TRUE)
  
  z3 <- md_autoplot_(x = x, xnm = xnm, ..., fig.height = ceiling(length(x)/2) * 3.5, fig.width = 10)
  
  c(z1, z2, z3) # ?fastmd::c.md_lines
  
}


#' @importFrom flextable wrap_flextable
as_flextable_quantile_survfitlist <- \(x, ...) {
  x |> 
    lapply(FUN = as_flextable_quantile_survfit) |>
    lapply(FUN = wrap_flextable) |>
    Reduce(f = `+`) + 
    plot_layout(ncol = 1L) # ?patchwork::plot_layout
}







