
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
#' survdiff(Surv(futime, fustat) ~ rx,data=ovarian) |>
#'  .pval.survdiff()
#' 
#' @importFrom stats pchisq
#' @export
.pval.survdiff <- function(x) {
  pchisq(x$chisq, df = length(x$n) - 1L, lower.tail = FALSE)
}



