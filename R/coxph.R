
#' @title S3 methods of \link[survival]{coxph}
#' 
#' @param x an \link[survival]{coxph} object  
#' 
#' @examples
#' ?survival::cox.zph
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
#' @export
desc_.coxph <- function(x) 'Cox proportional hazards'


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
