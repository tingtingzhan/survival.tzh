

#' @title S3 Methods \link[survival]{survdiff} Object
#' 
#' @param x \link[survival]{survdiff} object
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
#' @export
nobsText.survdiff <- function(x) {
  sprintf(fmt = '%d subj (%d events)', sum(x[['n']]), sum(x[['obs']]))
}


#' @name S3_survdiff
#' @export
.pval.survdiff <- function(x) x[['pvalue']]




