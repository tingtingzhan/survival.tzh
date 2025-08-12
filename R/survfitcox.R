


#' @title Kaplan-Meier Curve of `survfitcox` Objects using \CRANpkg{ggplot2}
#' 
#' @description ..
#' 
#' @param object `'survfitcox'` object, returned from `survival:::survfit.coxph`
#' 
#' @param ... ..
#' 
#' @examples 
#' library(ggplot2)
#' m1 = coxph(Surv(time, status) ~ celltype, data = veteran)
#' p = m1 |> survfit() |> autoplot()
#' p = m1 |> survfit(newdata = veteran[1:90, ]) |> autoplot()
#' @keywords internal
#' @importFrom ggplot2 autoplot ggplot scale_y_continuous labs
#' @importFrom stats formula
#' @export autoplot.survfitcox
#' @export
autoplot.survfitcox <- function(object, ...) {
  cox_cl <- object$call$formula
  if (!inherits(cox <- eval(cox_cl), what = 'coxph')) stop('cox model not evaluable?')
  if ((fom <- formula(cox))[[1L]] != '~') stop('cox model does not carry a real formula')
  data_cl <- if (length(newdata_cl <- object$call$newdata)) newdata_cl else cox$call$data
  
  ggKM_cl <- as.call(list(quote(ggKM.formula), formula = fom, data = data_cl, ...))
  # call(name = 'ggKM.formula', formula = fom, data = data_cl, ...) # wrong
  return(eval(ggKM_cl))
}



#' @title Endpoint of `'survfitcox'` Object
#' 
#' @param x an object of class `'survfitcox'`, returned from function `survival:::survfit.coxph()`
#' 
#' @examples
#' coxph(Surv(time, status) ~ celltype, data = veteran) |> 
#'   survfit() |>
#'   endpoint.survfitcox()
#' @keywords internal
#' @importFrom ecip endpoint endpoint.default
#' @export endpoint.survfitcox
#' @export
endpoint.survfitcox <- function(x) {
  cox <- tryCatch(eval(x$call$formula), error = as.null.default, warning = as.null.default)
  if (!length(cox) || !inherits(cox, what = 'coxph')) return('')
  return(endpoint.default(cox))
}






