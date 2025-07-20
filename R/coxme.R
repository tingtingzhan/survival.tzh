

#' @title S3 methods for \link[coxme]{coxme}
#' 
#' @param x \link[coxme]{coxme} object 
#' 
#' @examples 
#' library(coxme)
#' m = coxme(Surv(time, status) ~ ph.ecog + age + (1|inst), data = lung) 
#' m |> nobsText.coxme()
#' @name S3_coxme
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


#' @importFrom stats terms model.frame
#' @export
terms.coxme <- function(x, ...) {
  # coxme:::terms does not exists
  # stats:::terms.default does not return attr(, 'dataClasses')
  x |>
    model.frame() |> # activates ?stats::model.frame.default, as of packageDate('coxme') 2024-08-22
    # e.g.
    # list(Surv(time, status), ph.ecog, age, 1 | inst)
    # might not be correct hahaha
    attr(which = 'terms', exact = TRUE)
}

# ?MuMIn:::formula.coxme is different from ?coxme:::formula.coxme

# ?MuMIn:::logLik.coxme vs. ?coxme:::logLik.coxme


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
#' @importClassesFrom rmd.tzh md_lines
#' @importFrom methods new
#' @importFrom utils bibentry
#' @export
desc_.coxme <- function(x) {
  'mixed effects Cox [@Ripatti04; @Therneau03]' |> # ?coxme::coxme
    new(Class = 'md_lines', bibentry = c(
      bibentry(
        bibtype = 'Article', key = 'Ripatti04',
        author = 'Ripatti, Samuli and Palmgren, Juni',
        title = 'Estimation of Multivariate Frailty Models Using Penalized Partial Likelihood',
        journal = 'Biometrics',
        volume = '56',
        number = '4',
        pages = '1016-1022',
        year = '2004',
        month = '05',
        doi = '10.1111/j.0006-341X.2000.01016.x'
      ),
      bibentry(
        bibtype = 'Article', key = 'Therneau03',
        author = 'Terry M Therneau and Patricia M Grambsch and V. Shane Pankratz',
        title = 'Penalized Survival Models and Frailty',
        journal = 'Journal of Computational and Graphical Statistics',
        volume = '12',
        number = '1',
        pages = '156--175',
        year = '2003',
        doi = '10.1198/1061860031365'
      )
    ))
}



#' @rdname S3_coxme
#' @export
estnm.coxme <- function(x) 'Hazards\ Ratio'







