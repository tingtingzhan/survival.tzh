
desc_survdiff_rho <- function(rho, ...) {
  # see ?survival::survdiff
  if (missing(rho) || is.null(rho) || (rho == 0)) {
    # see ?survival::survdiff and ?coin::logrank_test
    # 'Log-rank [unweighted]' # 'Logrank [Mantel-Cox]' # 'hypergeometric variance'
    'Log-rank test'
  } else if (rho == 1) {
    'Peto-Gehan-Wilcoxon test'
  } else {
    'G-rho family of tests'
  }
}



#' @title S3 Methods \link[survival]{survdiff} Object
#' 
#' @param x a \link[survival]{survdiff} object
#' 
#' @note
#' Function `?broom:::nobs.survdiff` (2024-09-26) is **WRONG**.
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


#' @name S3_survdiff
#' @importFrom ecip .pval
#' @export .pval.survdiff
#' @export
.pval.survdiff <- function(x) x[['pvalue']]




#' @title [md_.survdiff]
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
#' ) |> rmd.tzh::render_(file = 'logrank')
#' @keywords internal
#' @importFrom methods new
#' @importClassesFrom rmd.tzh md_lines
#' @importFrom rmd.tzh md_ label_pvalue_sym
#' @export md_.survdiff
#' @export
md_.survdiff <- function(x, xnm, ...) {
  
  z1 <- sprintf(
    fmt = '%s on the time-to-event endpoint **`%s`** is performed (%s) using <u>**`R`**</u> package <u>**`survival`**</u>.',
    desc_survdiff_rho(x$call$rho),
    x$call$formula[[2L]] |> 
      deparse1(),
    x$pvalue |>
      label_pvalue_sym(add_p = TRUE)()
  ) |>
    new(Class = 'md_lines', package = 'survival')
  
  z2 <- c(
    '```{r}',
    '#| echo: false', 
    '#| comment: ',
    xnm,
    '```'
  ) |> 
    new(Class = 'md_lines')
  
  c(z1, z2) # ?rmd.tzh::c.md_lines
  
}


