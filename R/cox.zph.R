

#' @title S3 methods of \link[survival]{cox.zph}
#' 
#' @param x a \link[survival]{cox.zph} object
#' 
#' @examples
#' ?survival::cox.zph
#' m = coxph(Surv(futime, fustat) ~ age + ecog.ps, data = ovarian) |> cox.zph() 
#' .pval.cox.zph(m)
#' @name S3_coxzph
#' @export
.pval.cox.zph <- function(x) {
  if (!any(x$transform == c('log', 'km'))) stop('cox.zph transform is neither of log/km')
  x$table[, 'p']
}

#' @rdname S3_coxzph
#' @export
desc_.cox.zph <- function(x) 'Test Proportional Hazards Assumption of Cox Regression'
