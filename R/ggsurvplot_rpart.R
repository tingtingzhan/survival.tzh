
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
#' library(rpart)
#' rp = rpart(Surv(time, status) ~ age, data = veteran, maxdepth = 2L, model = TRUE)
#' library(rmd.tzh); list(
#'   'ggsurvplot_rpart' = rp |> ggsurvplot_rpart(pval.coord = c(750, .95))
#' ) |> render_(file = 'ggsurvplot_rpart')
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
    fit = sfit, data = sfit[['data']], 
    conf.int = TRUE,
    pval = TRUE, 
    legend = 'right', 
    ylab = deparse1(fit$terms[[2L]]), 
    ...)
  
  attr(p, which = 'text') = '@KaplanMeier58 estimates and curves based on the partition branches are created by <u>**`R`**</u> package <u>**`survminer`**</u>.' |>
    new(Class = 'md_lines', package = 'survminer', bibentry = bibentry(
      bibtype = 'article', key = 'KaplanMeier58',
      author = c('Edward L. Kaplan', 'Paul Meier'),
      title = 'Nonparametric Estimation from Incomplete Observations',
      journal = 'Journal of the American Statistical Association',
      volume = '53',
      number = '282',
      pages = '457--481',
      year = '1958',
      doi = '10.1080/01621459.1958.10501452'
    ))
  
  return(p)
  
}

#' @title md_.ggsurvplot
#' 
#' @param x ..
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom rmd.tzh md_ md_.default
#' @export md_.ggsurvplot
#' @export
md_.ggsurvplot <- md_.default # otherwise dispatch to ?rmd.tzh::md_.list



