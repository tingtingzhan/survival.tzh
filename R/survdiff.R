

#' @title Sample Size of \link[survival]{survdiff} Object
#' 
#' @param object \link[survival]{survdiff} object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @examples
#' m = survdiff(Surv(futime, fustat) ~ rx,data=ovarian)
#' stopifnot(nobs.survdiff(m) == nrow(ovarian))
#' @importFrom stats nobs
#' @export nobs.survdiff
#' @export
nobs.survdiff <- function(object, ...) sum(object$n)
# ?broom:::nobs.survdiff (2024-09-26) is WRONG!!!



#' @title Get \eqn{p}-values from \link[survival]{survdiff} Object
#' 
#' @param x \link[survival]{survdiff} object
#' 
#' @examples
#' m = survdiff(Surv(futime, fustat) ~ rx,data=ovarian)
#' .pval.survdiff(m)
#' 
#' @importFrom stats pchisq
#' @export
.pval.survdiff <- function(x) {
  pchisq(x$chisq, df = length(x$n) - 1L, lower.tail = FALSE)
}



