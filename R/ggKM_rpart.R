
#' @title Kaplan-Meier Curves based on \link[rpart]{rpart}
#' 
#' @description
#' ..
#' 
#' @param object \link[rpart]{rpart}
#' 
#' @param ... ..
#' 
#' @examples
#' library(rpart)
#' rp = rpart(Surv(time, status) ~ age, data = veteran, maxdepth = 2L, model = TRUE)
#' ggKM(rp)
#' @importFrom ggplot2 ggplot scale_y_continuous labs
#' @importFrom rpart.tzh survdiff_rpart survfit.rpart
#' @importFrom scales label_percent
#' @export ggKM.rpart
#' @export
ggKM.rpart <- function(object, ...) {
  
  model_ <- object$model
  if (is.null(model_) || !is.data.frame(model_)) stop('Re-run `rpart` with `model = TRUE`')
  y <- model_[[1L]] # units.Surv carries hahaha!!
  if (!inherits(y, what = 'Surv')) return(invisible()) # exception handling
  
  lab_ <- 'Recursive\nPartitioning'
  
  ggplot() +
    autolayer.survfit(object = survfit.rpart(object), ...) +
    scale_y_continuous(labels = label_percent()) +
    labs(x = units.Surv(y), 
         y = deparse1(object$terms[[2L]]),
         colour = lab_, fill = lab_#,
         #caption = paste(survdiff_rpart(object)$pvalue |> label_pvalue_sym(add_p = TRUE)(), 'Log-rank (unweighted)') # ?flextable.tzh::label_pvalue_sym
    )
  
}

