
# packageDate('ggfortify') 2024-04-16

#' @title Kaplan Meier Curve via \CRANpkg{ggplot2}
#' 
#' @description ..
#' 
#' @param object ..
#' 
#' @param formula \link[stats]{formula}
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param envir ..
#' 
#' @param ... additional parameters of function [autolayer.survfit()]
#' 
#' @details 
#' 
#' Function [ggKM()] is more elaborated than [autoplot.survfit()].
#' 
#' @returns 
#' 
#' Function [ggKM()] returns a \link[ggplot2]{ggplot} object.
#' 
#' @references 
#' \url{http://sape.inf.usi.ch/quick-reference/ggplot2/colour}
#' 
#' For function [ggKM()] \url{http://www.stat.unipg.it/luca/R/CumIncidence.R}
#' 
#' Package \pkg{ggfortify} \url{https://cran.r-project.org/web/packages/ggfortify/}
#' 
#' 
#' @name ggKM
#' @importFrom scales label_percent
#' @export
ggKM <- function(object, data, ...) UseMethod(generic = 'ggKM')

#' @rdname ggKM
#' @importFrom ggplot2 ggplot scale_y_continuous labs
#' @importFrom survival survfit.formula
# @importFrom scales.tzh label_pvalue_sym
#' @export ggKM.formula
#' @export
ggKM.formula <- function(formula, data, ..., envir = parent.frame()) {
  cl0 <- match.call()
  units <- units.Surv(eval(cl0$formula[[2L]], envir = data))
  sfit <- eval(call(name = 'survfit.formula', formula = cl0$formula, data = cl0$data), envir = envir)
  #p_survdiff <- if (formula[[3L]] != 1L) {
  #  sdiff <- eval(call(name = 'survdiff', formula = cl0$formula, data = cl0$data), envir = envir)
  #  paste(sdiff$pvalue |> label_pvalue_sym(add_p = TRUE)(), 'Log-rank (unweighted)')
  #} # else NULL
  p <- ggplot() + 
    autolayer.survfit(sfit, units = units, ...) + 
    scale_y_continuous(labels = label_percent()) +
    #labs(x = units, caption = p_survdiff)
    labs(x = units)
  attr(p, which = 'text') <- Sprintf.survfit(sfit)
  return(p)
}

#' @rdname ggKM
#' @export
ggKM.coxph <- function(object, ...) {
  # note that we have ?survival:::survfit.coxph
  
  # this is only the subjects **in the model**
  ggKM.formula(y ~ 1, data = data.frame(y = object$y)) +
    labs(y = deparse1(formula(object)[[2L]]))
}
