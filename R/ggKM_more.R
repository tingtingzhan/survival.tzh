


#' @title m_ggKM
#' 
#' @description
#' Array of Kaplan-Meier curves of multiple endpoints and same predictors
#' 
#' @param formula \link[stats]{formula}.  
#' E.g. `cbind(y1, y2) ~ x1 + x2 + x3` for
#' an array of Kaplan Meier curves of `y1 ~ x1 + x2 + x3` and `y2 ~ x1 + x2 + x3`
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param ncol \link[base]{integer} scalar, see \link[gridExtra]{arrangeGrob}
#' 
#' @param ... ..
#' 
#' @examples
#' # ?survival::rotterdam
#' rtd = within(rotterdam, expr = {
#'   rtime = structure(rtime/365, units = 'years', class = 'difftime')
#'   #rtime = structure(rtime/365, class = 'difftime')
#'   dtime = structure(dtime/365, units = 'years', class = 'difftime')
#'   #dtime = structure(dtime/365, class = 'difftime')
#'   rfs = Surv(rtime, event = recur)
#'   os = Surv(dtime, event = death)
#' })
#' # y-axis not the same
#' ggKM(rfs ~ chemo, data = rtd)
#' ggKM(os ~ chemo, data = rtd)
#' library(grid)
#' grid.newpage(); grid.draw(m_ggKM(cbind(rfs, os) ~ chemo, data = rtd))
#' @importFrom ggplot2 labs layer_scales 
#' @importFrom gridExtra arrangeGrob
#' @importFrom scales percent
#' @importFrom survival survfit.formula
#' @export
m_ggKM <- function(formula, data, ncol = 2L, ...) {
  
  cl <- match.call()
  
  if (formula[[2L]][[1L]] != 'cbind') stop()
  edps <- as.list.default(formula[[2L]][-1L])
  
  foms <- lapply(edps, FUN = function(edp) eval(call(name = '~', edp, formula[[3L]])))
  
  figs0 <- .mapply(FUN = function(formula) {
    eval(call(name = 'ggKM.formula', formula = formula, data = quote(data)))
  }, dots = list(formula = foms), MoreArgs = NULL)
  ylim_ <- range.default(lapply(figs0, FUN = function(p) layer_scales(p)$y$range$range))
  figs <- lapply(figs0, FUN = function(p) {
    suppressMessages(p + scale_y_continuous(limits = ylim_, labels = percent))
  }) 
  
  ret <- arrangeGrob(grobs = figs, ncol = ncol, ...)
  
  attr(ret, which = 'text') <- sprintf(
    fmt = 'Kaplan-Meier estimates and curves of time-to-event endpoints %s%s are obtained using <u>**`R`**</u> package <u>**`survival`**</u>.',
    paste0('`', edps, '`', collapse = ', '),
    if (identical(formula[[3L]], 1)) '' else {
      sprintf(fmt = ', by covariate(s) %s,', paste0('`', all.vars(formula[[3L]]), '`', collapse = ','))
    })
  
  attr(ret, which = 'formula') <- formula
  
  return(ret)
}




