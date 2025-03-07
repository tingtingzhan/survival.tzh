

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






#' @title Message on Proportional Hazards Assumption Test
#' 
#' @description ..
#' 
#' @param fit,global,... parameters of function \link[survival]{cox.zph}
#' 
#' @details 
#' Function [cox_zph] prints ...
#' 
#' @examples 
#' # ?coxphw::coxphw examples
#' data(gastric, package = 'coxphw')
#' coxph(Surv(time, status) ~ radiation, data = gastric) |> cox_zph()
#' @references
#' \url{http://www.sthda.com/english/wiki/cox-model-assumptions}
#' 
#' @importFrom survival cox.zph
#' @export
cox_zph <- function(fit, global = TRUE, ...) {
  
  zph <- fit |> cox.zph(global = global, ...)
  p <- zph |> .pval.cox.zph()
  
  if (length(p0 <- p[!is.na(p)]) && any(id <- (p0 <= .05))) {
    message('Proportional hazard assumption violated (Schoenfeld residuals):')
    print(zph) # ?survival:::print.cox.zph
  }

  return(invisible(fit))
}













