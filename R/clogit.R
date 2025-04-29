

#' @title Additional S3 methods for \link[survival]{clogit} Objects
#' 
#' @param x \link[survival]{clogit} object
#' 
#' @examples
#' # see ?survival::clogit
#' library(survival)
#' data(logan, package = 'survival')
#' resp = levels(logan$occupation)
#' n = nrow(logan)
#' indx = rep(1:n, length(resp))
#' logan2 = data.frame(logan[indx,], id = indx, tocc = factor(rep(resp, each=n)))
#' logan2$case = (logan2$occupation == logan2$tocc)
#' (m = clogit(case ~ tocc + tocc:education + strata(id), data = logan2))
#' endpoint.clogit(m)
#' 
#' @name clogit_S3
#' @export
endpoint.clogit <- function(x) {
  # do not use ?tzh::endpoint.default
  # stats::formula dispatches to ?stats:::formula.default
  # I do not want to write [formula.clogit], may mess up with \pkg{survival}
  fom <- x$userCall[[2L]]
  if (!is.call(fom) || (fom[[1L]] != '~')) stop('user formula not formula?')
  return(fom[[2L]]) # tzh::endpoint.formula
}


#' @rdname clogit_S3
#' @export
estnm.clogit <- function(x) 'Odds\ Ratio'



#' @rdname clogit_S3
#' @export
desc_.clogit <- function(x) 'conditional logistic regression (via Cox model)'
