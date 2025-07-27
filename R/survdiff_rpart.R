



#' @title Test Survival Curve Differences based on \link[rpart]{rpart}
#' 
#' @description
#' ..
#' 
#' @param object \link[rpart]{rpart}
#' 
#' @param ... ..
#' 
#' @importFrom survival survdiff
#' @export
survdiff_rpart <- function(object, ...) {
  
  model_ <- object$model
  if (is.null(model_) || !is.data.frame(model_)) stop('Re-run `rpart` with `model = TRUE`')
  y <- model_[[1L]] # units.Surv carries hahaha!!
  if (!inherits(y, what = 'Surv')) return(invisible()) # exception handling
  
  leafRisk <- risklev(object, ...)
  
  return(survdiff(y ~ leafRisk))
  
} 


