


#' @title Kaplan-Meier Curve of `survfitcox` Objects using \CRANpkg{ggplot2}
#' 
#' @description ..
#' 
#' @param object `'survfitcox'` object, returned from `survival:::survfit.coxph`
#' 
#' @param ... ..
#' 
#' @examples 
#' m1 = coxph(Surv(time, status) ~ celltype, data = veteran)
#' m1 |> survfit() |> autoplot()
#' m1 |> survfit(newdata = veteran[1:90, ]) |> autoplot()
#' m1 |> survfit(newdata = veteran[1:90, ]) |> autoplot(times = c(250))
#' @keywords internal
#' @importFrom stats formula
#' @importFrom ggplot2 ggplot scale_y_continuous labs
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



# @title Additional S3 methods for `survfitcox` Objects
# 
# @param x `survfitcox` object
# 
# @name S3_survfitcox
# @export
#endpoint.survfitcox <- function(x) {
#  cox <- tryCatch(eval(x$call$formula), error = as.null.default, warning = as.null.default)
#  if (!length(cox) || !inherits(cox, what = 'coxph')) return('')
#  #return(endpoint.default(cox))
#  NextMethod(generic = 'endpoint', object = cox) # currently dispatch to tzh::endpoint.default
# hahah, does not work :)
#}




