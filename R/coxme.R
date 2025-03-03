

#' @title S3 methods for \link[coxme]{coxme}
#' 
#' @param x \link[coxme]{coxme} object 
#' 
#' @examples 
#' library(coxme)
#' m = coxme(Surv(time, status) ~ ph.ecog + age + (1|inst), data = lung) 
#' m |> .pval.coxme()
#' m |> nobsText.coxme()
#' @name S3_coxme
#' @export
.pval.coxme <- function(x) {
  x |> 
    summary() |> # ?coxme:::summary.coxme (return is `x`)
    .pval.summary.coxme()
}

#' @rdname S3_coxme
#' @export
.pval.summary.coxme <- function(x) {
  cf <- x$coefficients
  ret <- cf[, 'p']
  names(ret) <- rownames(cf) # nrow-1 drops rownames
  return(ret)
}



#' @rdname S3_coxme
#' @export
nobsText.coxme <- function(x) {
  ng <- lengths(x$frail, use.names = TRUE)
  n <- x[['n']]
  sprintf(fmt = '%d records (%d events) from %s', 
          n[2L], n[1L], # inspired by ?MuMIn:::nobs.coxme
          paste(sprintf(fmt = '%d `%s`', ng, names(ng)), collapse = ' nested in '))
}




# ?MuMIn:::formula.coxme is different from ?coxme:::formula.coxme

# ?MuMIn:::logLik.coxme vs. ?coxme:::logLik.coxme

#' @rdname S3_coxme
#' @export
expcoef.coxme <- function(x) TRUE


# @method model.frame coxme
# @importFrom stats model.frame model.frame.default
# @export
#model.frame.coxme <- function(formula, ...) {
#  if (!is.data.frame(data <- eval(formula$call$data))) stop('`data` must be evaluable')
#  model.frame.default(formula(formula), data = data, ...)
#} # do I still need this as of 2025 Spring?



# do not overwrite ?coxme:::vcov.coxme, although which is slow and not stringent
# @export
#vcov.coxme <- function(object, ...) {
#  cf <- object$coefficients # ?coxme:::fixef.coxme
#  vv <- as.matrix(object$variance) # was S4 'bdsmatrix'
#  indx <- seq.int(length.out = length(cf), to = dim(vv)[[1L]])
#  y <- vv[indx, indx, drop = FALSE]
#  dimnames(y) <- list(nm <- names(cf), nm)
#  return(y)
#}


#' @rdname S3_coxme
#' @export
desc_.coxme <- function(x) 'mixed effects Cox' # ?coxme::coxme

#' @rdname S3_coxme
#' @export
estName.coxme <- function(x) 'Hazards\ Ratio'







