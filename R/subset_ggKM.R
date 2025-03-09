

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
#' @examples
#' subset_ggKM(os ~ ph.ecog, subset = list(
#'   All = TRUE, Female = (sex == 'Female'), Male = (sex == 'Male')
#' ), data = lung2)
#' 
#' id = quote(list(
#'   All = TRUE, Female = (sex == 'Female'), Male = (sex == 'Male')
#' ))
#' subset_ggKM(os ~ ph.ecog, subset = id, data = lung2)
#' @importFrom ggplot2 labs layer_scales
#' @importFrom scales percent
#' @export
subset_ggKM <- function(formula, subset, data, ...) {
  
  e_ <- tryCatch(subset, error = identity)
  if (inherits(e_, what = 'error')) e_ <- substitute(subset)
  e <- e_ |> as.list.default()
  nm <- names(e[-1L])
  if (!length(nm) || anyNA(nm) || !all(nzchar(nm))) stop('`subset` must be fully named')
  
  d <- lapply(e[-1L], FUN = function(e_) {
    do.call(what = 'subset.data.frame', args = list(x = data, subset = e_))
  })
  
  nr <- vapply(d, FUN = .row_names_info, type = 2L, FUN.VALUE = NA_integer_)
  
  tt <- sprintf(fmt = '%s, n=%d', nm, nr) # title (with sample size)

  p0 <- lapply(d, FUN = function(d) {
    do.call(what = 'ggKM.formula', args = list(formula = formula, data = d, ...))
  })
  
  yl <- range.default(lapply(p0, FUN = function(p) layer_scales(p)$y$range$range))
  
  suppressMessages(p1 <- mapply(FUN = function(p, tt) {
    p + 
      scale_y_continuous(labels = percent, limits = yl) + 
      labs(title = tt)
  }, p = p0, tt = tt, SIMPLIFY = FALSE))
  
  return(Reduce(f = `+`, x = p1)) # 'patchwork'
  # tzh do not know how \pkg{patchwork} renews definition of function `+`
  # therefore, tzh simply @import patchwork, for now
  
}




  
