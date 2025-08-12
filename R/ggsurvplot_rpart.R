
#' @title Kaplan-Meier Curves of \link[rpart]{rpart} via \CRANpkg{survminer}
#' 
#' @description
#' Kaplan-Meier curves based on recursive partitioning and regression trees \link[rpart]{rpart}.
#' 
#' @param fit \link[rpart]{rpart}
#' 
#' @param ... ..
#' 
#' @examples 
#' rp = rpart::rpart(Surv(time, status) ~ age, data = veteran, maxdepth = 2L, model = TRUE)
#' list(
#'   'ggsurvplot_rpart' = rp |> ggsurvplot_rpart(pval.coord = c(750, .95))
#' ) |> rmd.tzh::render_(file = 'ggsurvplot_rpart')
#' 
#' @keywords internal
#' @importFrom survminer ggsurvplot
#' @importFrom rpart.tzh risklev
#' @importClassesFrom rmd.tzh md_lines
#' @export
ggsurvplot_rpart <- function(fit, ...) {
  # works but too ugly
  
  sfit <- survfit.rpart(fit)
  if (!length(sfit)) return(invisible())
  
  p <- ggsurvplot(
    fit = sfit, 
    data = sfit[['data']], 
    conf.int = TRUE,
    pval = TRUE, 
    #legend = 'right',
    risk.table = TRUE,
    ylab = deparse1(fit$terms[[2L]]), 
    ...)
  
  attr(p, which = 'text') <- '@KaplanMeier58 estimates and curves based on the recursive partitioning branches are created by <u>**`R`**</u> package <u>**`rpart`**</u> and <u>**`survminer`**</u>.' |>
    new(Class = 'md_lines', package = c('rpart', 'survminer'), bibentry = KaplanMeier58())
  
  attr(p, which = 'fig-height') <- 7
  
  return(p)
  
}

#' @title md_.ggsurvplot
#' 
#' @param x ..
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom rmd.tzh md_ md_.default
#' @export md_.ggsurvplot
#' @export
md_.ggsurvplot <- md_.default # otherwise dispatch to ?rmd.tzh::md_.list



