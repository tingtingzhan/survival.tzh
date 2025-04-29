
#' @title S3 methods for \link[survival]{survreg}
#' 
#' @param x a \link[survival]{survreg} or \link[survival]{summary.survreg}
#' 
#' @examples
#' library(survival)
#' # ?survival::survreg
#' m = survreg(Surv(futime, fustat) ~ ecog.ps + rx, data = ovarian, dist = 'weibull', scale = 1) 
#' 
#' @name S3_survreg
#' @export
.pval.summary.survreg <- function(x) {
  # ?survival:::summary.survreg
  ret <- x$table[, 'p']
  names(ret) <- rownames(x$table)
  return(ret)
}


