
#' @title S3 methods of \link[survival]{coxph}
#' 
#' @param x an \link[survival]{coxph} object  
#' 
#' @examples
#' ?survival::cox.zph
#' library(survival)
#' m = ovarian |>
#'  within.data.frame(expr = {
#'   ecog.ps = factor(ecog.ps)
#'  }) |>
#'  coxph(formula = Surv(futime, fustat) ~ age + ecog.ps)
#' survival:::nobs.coxph(m) # number of events!!
#' # MuMIn:::nobs.coxph(m) # also number of events
#' nobsText.coxph(m)
#' @name S3_coxph
#' @export
nobsText.coxph <- function(x) {
  sprintf(fmt = '%d subj (%d events)', x[['n']], x[['nevent']])
}


#' @rdname S3_coxph
#' @importClassesFrom rmd.tzh md_lines
#' @importFrom methods new
#' @importFrom utils bibentry
#' @export
desc_.coxph <- function(x) {
  '@Cox72 proportional hazards' |>
    new(Class = 'md_lines', bibentry = bibentry(
      bibtype = 'Article', key = 'Cox72',
      author = 'David R. Cox',
      title = 'Regression Models and Life-Tables',
      journal = 'Journal of the Royal Statistical Society: Series B (Methodological)',
      volume = '34',
      number = '2',
      pages = '187-202',
      doi = '10.1111/j.2517-6161.1972.tb00899.x',
      year = '1972'
    ))
}

#' @rdname S3_coxph
#' @export
estnm.coxph <- function(x) 'Hazards\ Ratio'


#' @rdname S3_coxph
#' @export
.pval.summary.coxph <- function(x) {
  ret <- x$coefficients[, 'Pr(>|z|)']
  names(ret) <- rownames(x$coefficients)
  return(ret)
} 
