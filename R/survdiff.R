
#' @importFrom stats nobs
#' @export
nobs.survdiff <- function(object, ...) sum(object$n)
# ?broom:::nobs.survdiff (2024-09-26) is WRONG!!!




