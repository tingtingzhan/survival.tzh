
#' @title \link[survival]{coxph} Objects
#' 
#' @examples
#' # ?survival::cox.zph
#' m = ovarian |>
#'  within.data.frame(expr = {
#'   ecog.ps = factor(ecog.ps)
#'  }) |>
#'  coxph(formula = Surv(futime, fustat) ~ age + ecog.ps)
#' 
#' library(ecip); list(
#'  '`coxph`' = m
#' ) |> fastmd::render2html()
#' 
#' @name coxph
NULL



#' @importFrom ecip nobsText
#' @export
nobsText.coxph <- function(x) {
  sprintf(
    fmt = '%d subj (%d events)', 
    x[['n']], 
    x[['nevent']])
}


#' @importClassesFrom fastmd md_lines
#' @importFrom ecip desc_
#' @export
desc_.coxph <- function(x) {
  '@Cox72 proportional hazards' |>
    new(Class = 'md_lines', bibentry = .cox72(), package = 'survival')
}

#' @importFrom ecip estnm
#' @export
estnm.coxph <- function(x) 'Hazards\ Ratio'



#' @importFrom ecip .pval
#' @method .pval summary.coxph
#' @export
.pval.summary.coxph <- function(x) {
  ret <- x$coefficients[, 'Pr(>|z|)']
  names(ret) <- rownames(x$coefficients)
  return(ret)
} 


#' @importFrom fastmd md_
#' @importFrom ecip md_ecip
#' @export
md_.coxph <- md_ecip

