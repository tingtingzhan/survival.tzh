

#' @title \link[survival]{clogit} Objects
#' 
#' @examples
#' # see ?survival::clogit
#' resp = levels(survival::logan$occupation)
#' n = nrow(logan)
#' indx = rep(1:n, length(resp))
#' logan2 = data.frame(logan[indx,], id = indx, tocc = factor(rep(resp, each=n)))
#' logan2$case = (logan2$occupation == logan2$tocc)
#' m = clogit(case ~ tocc + tocc:education + strata(id), data = logan2)
#' library(ecip); list(
#'  '`clogit`' = m
#' ) |> fastmd::render2html()
#' 
#' @name clogit
NULL



#' @importFrom ecip endpoint
#' @export
endpoint.clogit <- function(x) {
  # do not use ?ecip:::endpoint.default
  # stats::formula dispatches to ?stats:::formula.default
  # I do not want to write [formula.clogit], may mess up with \pkg{survival}
  fom <- x$userCall[[2L]]
  if (!is.call(fom) || (fom[[1L]] != '~')) stop('user formula not formula?')
  fom |> 
    endpoint() # ecip:::endpoint.formula()
}


#' @importFrom ecip estnm
#' @export
estnm.clogit <- function(x) 'Odds\ Ratio'



#' @importFrom ecip desc_
#' @export
desc_.clogit <- function(x) {
  'conditional logistic regression (via Cox model)' |>
    sprintf(fmt = '*%s*') |>
    new(Class = 'md_lines', package = 'survival')
}
