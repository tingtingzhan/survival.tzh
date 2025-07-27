
#' @title Survival Curves based on \link[rpart]{rpart}
#' 
#' @description
#' ..
#' 
#' @param formula \link[rpart]{rpart}
#' 
#' @param ... ..
#' 
#' @examples
#' library(rpart)
#' rp = rpart(Surv(time, status) ~ age, data = veteran, maxdepth = 2L, model = TRUE)
#' rp |> survfit.rpart()
#' 
#' @importFrom survival survfit survfit.formula
#' @export survfit.rpart
#' @export
survfit.rpart <- function(formula, ...) {
  
  object <- formula; formula <- NULL
  
  # ?rpart::rpart.exp changes 'Surv' endpoint `object$y` to 'matrix'
  # I need to read more (about why this is necessary), before writing to the authors
  
  model_ <- object$model
  if (is.null(model_) || !is.data.frame(model_)) stop('Re-run `rpart` with `model = TRUE`')
  y <- model_[[1L]] # units.Surv carries hahaha!!
  if (!inherits(y, what = 'Surv')) return(invisible()) # exception handling
  
  leafRisk <- risklev(object, ...)
  
  ret <- survfit.formula(y ~ leafRisk)
  ret$data <- data.frame(y = y, leafRisk = leafRisk) # for ?survminer::ggsurvplot
  return(ret)
}



