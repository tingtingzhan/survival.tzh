
# The `S3` methods that should go to \pkg{survival}


# @note
# Without the function [summary.Surv()], 
# the `S3` generic function \link[base]{summary} dispatches to function \link[base]{summary.default}
# if the input is a \link[survival]{Surv} object.
if (FALSE) {
  survival::aml |>
    within.data.frame(expr = {
      os = survival::Surv(time = time, event = status)
      time = status = NULL
    }) |>
    summary()
  
  survival::heart |>
    within.data.frame(expr = {
      os = survival::Surv(time = start, time2 = stop, event = event)
      start = stop = event = NULL
    }) |>
    summary()
} # examples for [summary.Surv()]

#' @export
summary.Surv <- function(object, ...) {
  
  # read ?base::summary.data.frame very carefully!!
  
  z <- if (ncol(object) == 2L) {
    (object[,2L] + 1L) |>
      structure(levels = c('[right-censored]', '[observed]'), class = 'factor') |>
      summary.factor()
  } else if (ncol(object) == 3L) {
    return(NextMethod(generic = 'summary')) # tzh doesnt know left-censoring and interval-censoring too well..
  } else stop('shouldnt happen')
  
  return(c(
    '<time-to-event>' = '(Surv)',
    z
  ))
  
}


# writing
# [cbind.Surv()] 
# won't help ?stats:::aggregate.formula
# only option is to avoid ?stats:::aggregate.formula !!!