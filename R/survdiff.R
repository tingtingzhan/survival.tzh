
desc_survdiff_rho <- function(rho, ...) {
  # see ?survival::survdiff
  if (missing(rho) || is.null(rho) || (rho == 0)) {
    # see ?survival::survdiff and ?coin::logrank_test
    # 'Log-rank [unweighted]' # 'Logrank [Mantel-Cox]' # 'hypergeometric variance'
    '[Log-rank test](https://en.wikipedia.org/wiki/Logrank_test)'
  } else if (rho == 1) {
    '@Petos72\'s modification of @Gehan65 test'
  } else {
    '@HarringtonFleming82 $G^{\\rho,\\gamma}$ family of tests'
  }
}



#' @title S3 Methods \link[survival]{survdiff} Object
#' 
#' @param x a \link[survival]{survdiff} object
#' 
#' @note
#' The `S3` method `broom:::nobs.survdiff()` (2026-01-26) is **WRONG**.
#' 
#' @examples
#' m = survdiff(Surv(futime, fustat) ~ rx, data = ovarian) 
#' m |> nobsText.survdiff()
#' m |> .pval.survdiff()
#'  
#' @name S3_survdiff
#' @keywords internal
#' @importFrom ecip nobsText
#' @export nobsText.survdiff
#' @export
nobsText.survdiff <- function(x) {
  sprintf(fmt = '%d subj (%d events)', sum(x[['n']]), sum(x[['obs']]))
}


#' @rdname S3_survdiff
#' @importFrom ecip .pval
#' @export .pval.survdiff
#' @export
.pval.survdiff <- function(x) x[['pvalue']]




#' @title Fast Markdown Lines for \link[survival]{survdiff} Object
#' 
#' @description ..
#' 
#' @param x a \link[survival]{survdiff} object
#' 
#' @param xnm ..
#'  
#' @param ... ..
#' 
#' @examples
#' list(
#'  logrank = survdiff(Surv(time, status) ~ x, data = aml)
#' ) |> fastmd::render2html(file = 'logrank')
#' @keywords internal
#' @importClassesFrom fastmd md_lines
#' @importFrom fastmd md_ md_.default label_pvalue_sym
#' @export md_.survdiff
#' @export
md_.survdiff <- function(x, xnm, ...) {
  
  z1 <- sprintf(
    fmt = '%s on the time-to-event endpoint **`%s`** by %s (%s) is performed using <u>**`R`**</u> package <u>**`survival`**</u>.',
    desc_survdiff_rho(x$call$rho),
    x$call$formula[[2L]] |> 
      deparse1(),
    x$call$formula[[3L]] |>
      all.vars() |>
      sprintf(fmt = '`%s`') |>
      paste(collapse = ', '),
    x |>
      .pval.survdiff() |>
      label_pvalue_sym(add_p = TRUE)()
  ) |>
    new(Class = 'md_lines', package = 'survival', bibentry = c(
      .harrington_fleming82(),
      .petos72(),
      .gehan65()
    ))
  
  z2 <- if (!missing(xnm)) {
    md_.default(x, xnm = xnm, ...)
  } # else NULL
  
  c(z1, z2) # ?fastmd::c.md_lines
  
}



#' @title S3 Generic Function [survdiff_()]
#' 
#' @description
#' An `S3` generic function for downstream packages
#' 
#' 
#' @param object an R object
#' 
#' @param ... additional parameters of the workhorse function \link[survival]{survdiff}
#' 
#' @details
#' The `S3` methods of the generic function [survdiff_()]
#' applies the workhorse function \link[survival]{survdiff} to different R objects
#' 
#' 
#' @keywords internal
#' @export
survdiff_ <- function(object, ...) UseMethod(generic = 'survdiff_')
