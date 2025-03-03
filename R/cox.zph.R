

#' @title S3 methods of \link[survival]{cox.zph}
#' 
#' @param x a \link[survival]{cox.zph} object
#' 
#' @returns
#' Function [.pval.cox.zph] returns a \link[base]{double} \link[base]{vector}.
#' 
#' @examples
#' ?survival::cox.zph
#' m = coxph(Surv(futime, fustat) ~ age + ecog.ps, data = ovarian) |> cox.zph() 
#' .pval.cox.zph(m)
#' @name S3_coxzph
#' @export
.pval.cox.zph <- function(x) {
  if (!any(x$transform == c('log', 'km'))) stop('cox.zph transform is neither of log/km')
  cf <- x$table
  ret <- cf[, 'p']
  names(ret) <- rownames(cf) # nrow-1 drops rownames
  return(ret)
}

# @rdname S3_coxzph
# @export
# enduser wont interact with 'cox.zph'
#desc_.cox.zph <- function(x) 'Test Proportional Hazards Assumption of Cox Regression'
