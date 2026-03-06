
#' @title \link[survival]{survreg} Object
#' 
#' @examples
#' # ?survival::survreg
#' m = survreg(Surv(futime, fustat) ~ ecog.ps + rx, data = ovarian, dist = 'weibull', scale = 1) 
#' 
#' @name survreg
NULL



#' @importFrom ecip .pval
#' @method .pval summary.survreg
#' @export
.pval.summary.survreg <- function(x) {
  # ?survival:::summary.survreg
  ret <- x$table[, 'p']
  names(ret) <- rownames(x$table)
  return(ret)
}


