




#' @title One-Minus Kaplan-Meier Estimates
#' 
#' @param x ..
#' 
#' @keywords internal
#' @name oneminus
#' @export
oneminus.survfit <- function(x) {
  .Defunct(msg = 'do not use this!  survival:::summary.survfit(oneminus.survfit(.)) is unpredictable')
  x$surv <- 1 - x$surv
  x$lower <- 1 - x$lower
  x$upper <- 1 - x$upper
  # x$cumhaz <- -x$cumhaz # ???
  return(x)
}

#' @rdname oneminus
#' @export
oneminus.summary.survfit <- function(x) {
  
  # look carefully at 
  # survival:::summary.survfit(., data.frame = FALSE)
  # survival:::summary.survfit(., data.frame = TRUE)
  # we only care about the shared elements!!
  
  # names(x) # as of packageDate('survival') 2024-12-17
  
  # x$time: do **not** change
  # x$n.risk; x$n.event; x$n.censor: do **not** change
  
  x$surv <- 1 - x$surv # this is now 'percentage died'
  
  # x$std.err: do **not** change
  
  # x$cumhaz <- -x$cumhaz # what should tzh do?
  # plot(x$cumhaz + x$surv) # what does this mean???
  
  # x$std.chaz: do **not** change
  # x$strata: do **not** change
  
  x$lower <- 1 - x$lower
  x$upper <- 1 - x$upper
  
  return(x)
  
}


