


#' @title Kaplan Meier Curves of Multiple Endpoints
#' 
#' @description
#' Array of Kaplan-Meier curves of multiple endpoints and same predictors.
#' 
#' @param formula \link[stats]{formula}.  
#' E.g. `cbind(y1, y2) ~ x1 + x2 + x3` for
#' an array of Kaplan Meier curves of `y1 ~ x1 + x2 + x3` and `y2 ~ x1 + x2 + x3`
#' 
#' @param ... additional parameters of [ggKM.formula()] 
#' 
#' @examples
#' # y-axis not the same
#' ggKM(rfs ~ chemo, data = rotterdam2)
#' ggKM(os ~ chemo, data = rotterdam2)
#' m_ggKM(cbind(rfs, os) ~ chemo, data = rotterdam2)
#' @importFrom ggplot2 labs layer_scales 
#' @importFrom scales label_percent
#' @export
m_ggKM <- function(formula, ...) { # ncol = 2L, 
  
  cl <- match.call()
  
  if (formula[[2L]][[1L]] != 'cbind') stop()
  
  p0 <- as.list.default(formula[[2L]][-1L]) |>
    lapply(FUN = \(edp) eval(call(name = '~', edp, formula[[3L]]))) |>
    lapply(FUN = \(fom) do.call(what = 'ggKM.formula', args = list(formula = fom, ...)))
  
  yl <- p0 |>
    lapply(FUN = \(p) layer_scales(p)$y$range$range) |>
    range.default()
  
  suppressMessages(p1 <- lapply(p0, FUN = \(p) {
    p + scale_y_continuous(labels = label_percent(), limits = yl)
  })) 
  
  return(Reduce(f = `+`, x = p1)) # 'patchwork'
  
}






