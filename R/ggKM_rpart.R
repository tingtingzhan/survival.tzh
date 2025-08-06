
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
#' rp = rpart::rpart(Surv(time, status) ~ age, data = veteran, maxdepth = 2L, model = TRUE)
#' library(rmd.tzh); list(
#'   'ggKM.rpart' = ggKM(rp)
#' ) |> render_(file = 'ggKM.rpart')
#' 
#' @keywords internal
#' @importFrom ggplot2 ggplot scale_y_continuous labs
#' @importFrom scales label_percent
#' @importFrom rmd.tzh label_pvalue_sym
#' @importClassesFrom rmd.tzh md_lines
#' @export ggKM.rpart
#' @export
ggKM.rpart <- function(object, ...) {
  
  model_ <- object$model
  if (is.null(model_) || !is.data.frame(model_)) stop('Re-run `rpart` with `model = TRUE`')
  y <- model_[[1L]] # units.Surv carries hahaha!!
  if (!inherits(y, what = 'Surv')) return(invisible()) # exception handling
  # since packageDate('rpart') 2025-01-06
  # \link[rpart]{rpart} return does not contain `y` even if `y = TRUE` is called ..
  # x |> terms() |> attr(which = 'dataClasses', exact = TRUE) gives 'nmatrix.2', not 'Surv'
  
  lab_ <- 'Recursive\nPartitioning'
  
  p <- ggplot() +
    autolayer.survfit(object = survfit.rpart(object), ...) +
    scale_y_continuous(labels = label_percent()) +
    labs(x = units.Surv(y), 
         y = deparse1(object$terms[[2L]]),
         colour = lab_, fill = lab_,
         caption = survdiff_rpart(object)$pvalue |> label_pvalue_sym(add_p = TRUE)() |> paste('Log-rank (unweighted)') 
    )
  
  attr(p, which = 'text') <- '@KaplanMeier58 estimates and curves based on the partition branches are created by <u>**`R`**</u> package <u>**`survival`**</u>.' |>
    new(Class = 'md_lines', package = 'survival', bibentry = KaplanMeier58())
  
  return(p)
  
}



