
#' @title S3 methods of \link[survival]{coxph}
#' 
#' @param x an \link[survival]{coxph} object  
#' 
#' @name S3_coxph
#' @keywords internal
#' @importFrom ecip nobsText
#' @export nobsText.coxph
#' @export
nobsText.coxph <- function(x) {
  sprintf(fmt = '%d subj (%d events)', x[['n']], x[['nevent']])
}


#' @rdname S3_coxph
#' @importClassesFrom fastmd md_lines
#' @importFrom ecip desc_
#' @importFrom methods new
#' @export desc_.coxph
#' @export
desc_.coxph <- function(x) {
  '@Cox72 proportional hazards' |>
    new(Class = 'md_lines', bibentry = Cox72(), package = 'survival')
}

#' @rdname S3_coxph
#' @importFrom ecip estnm
#' @export estnm.coxph
#' @export
estnm.coxph <- function(x) 'Hazards\ Ratio'


#' @rdname S3_coxph
#' @importFrom ecip .pval
#' @method .pval summary.coxph
#' @export .pval.summary.coxph
#' @export
.pval.summary.coxph <- function(x) {
  ret <- x$coefficients[, 'Pr(>|z|)']
  names(ret) <- rownames(x$coefficients)
  return(ret)
} 


#' @title R Markdown Lines for \link[survival]{coxph}
#' 
#' @param x,xnm,... ..
#' 
#' @examples
#' ?survival::cox.zph
#' m = ovarian |>
#'  within.data.frame(expr = {
#'   ecog.ps = factor(ecog.ps)
#'  }) |>
#'  coxph(formula = Surv(futime, fustat) ~ age + ecog.ps)
#' 
#' library(ecip); 
#' list('`coxph`' = m) |> fastmd::render_(file = 'coxph')
#' 
#' @keywords internal
#' @importFrom fastmd md_
#' @importFrom ecip md_ecip
#' @export md_.coxph
#' @export
md_.coxph <- md_ecip

