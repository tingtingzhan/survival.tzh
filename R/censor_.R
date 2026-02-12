

#' @title Censor a \link[survival]{Surv} Object
#' 
#' @description ..
#' 
#' @param x \link[survival]{Surv} object
#' 
#' @param censor \link[base]{numeric} scalar, censoring time
#' 
#' @details 
#' In practice, we sometimes want to ignore events happening after a time point.
#' 
#' @examples 
#' x = lung |> with(expr = Surv(time = time, event = (status == 2)))
#' head(x, n = 10)
#' x1 = censor_(x, censor = 400)
#' head(x1, n = 10)
#' @keywords internal
#' @export
censor_ <- function(x, censor) {
  if (!inherits(x, what = 'Surv')) stop('1st argument must be `Surv`')
  if (!is.numeric(censor) || length(censor) != 1L || is.na(censor) || censor < 0) stop('illegal censoring time')
  
  id <- (x[,1L] > censor)
  x[id,1L] <- censor
  x[id,2L] <- 0 # all alive at time `censor` is considered as being censored at `censor`
  return(x)
}


