

#' @title subset_ggKM
#' 
#' @description
#' 
#' Produce a \link[patchwork]{patchwork} of Kaplan-Meier curves for *subset analysis*.
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
#' Function [subset_ggKM()] returns a \link[patchwork]{patchwork}.
#' 
#' @importFrom ggplot2 labs layer_scales
#' @importFrom scales label_percent
#' @export
subset_ggKM <- function(formula, subset, data, ...) {
  
  e_ <- tryCatch(subset, error = identity)
  if (inherits(e_, what = 'error')) e_ <- substitute(subset)
  e <- e_ |> as.list.default()
  nm <- names(e[-1L])
  if (!length(nm) || anyNA(nm) || !all(nzchar(nm))) stop('`subset` must be fully named')
  
  d <- e[-1L] |>
    lapply(FUN = \(e_) {
      do.call(what = 'subset.data.frame', args = list(x = data, subset = e_))
    })
  
  nr <- d |>
    vapply(FUN = .row_names_info, type = 2L, FUN.VALUE = NA_integer_)
  
  tt <- sprintf(fmt = '%s, n=%d', nm, nr) # title (with sample size)

  p0 <- d |>
    lapply(FUN = \(d) {
      do.call(what = 'ggKM.formula', args = list(formula = formula, data = d, ...))
    })
  
  yl <- p0 |>
    lapply(FUN = \(p) layer_scales(p)$y$range$range) |>
    range.default()
  
  suppressMessages(p1 <- mapply(FUN = \(p, tt) {
    p + 
      scale_y_continuous(labels = label_percent(), limits = yl) + 
      labs(title = tt)
  }, p = p0, tt = tt, SIMPLIFY = FALSE))
  
  #return(p1)
  
  return(Reduce(f = `+`, x = p1)) # 'patchwork'
  # tzh do not know how \pkg{patchwork} renews definition of function `+`
  # therefore, tzh simply @import patchwork, for now
  
}




  
