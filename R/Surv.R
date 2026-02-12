

#' @title `S3` Generic Function [is.Surv.endpoint()]
#' 
#' @description
#' An `S3` generic function for downstream packages,
#' to determine if an R object is a model with a \link[survival]{Surv} endpoint
#' 
#' @param object an R object
#' 
#' @keywords internal
#' @export
is.Surv.endpoint <- function(object) UseMethod(generic = 'is.Surv.endpoint')


# ?survival:::duplicated.Surv is good (using ?survival:::as.matrix.Surv)
# ?survival:::unique.Surv good



#' @title Time Unit of a \link[survival]{Surv} object
#' 
#' @description ..
#' 
#' @param x \link[survival]{Surv} object
#' 
#' @seealso 
#' \link[base]{units}, `Hmisc:::units.Surv`
#' 
#' @keywords internal
#' @export units.Surv
#' @export
units.Surv <- function(x) {
  at <- attributes(x)
  if (length(at$units)) stop('I have retired this usage')
  if (!length(ia <- at$inputAttributes)) return(invisible())
  un <- ia$time$units
  un2 <- ia$time2$units
  if (length(un)) {
    if (length(un2) && (un != un2)) stop('`time` and `time2` in Surv not having same unit')
    return(un)
  }
  if (length(un2)) return(un2)
  return(invisible())
}

#' @rdname more_units_set
#' @export more_units<-.Surv
#' @export
`more_units<-.Surv` <- function(x, value) {
  y <- x
  if (length(un <- units.Surv(x))) { # nomenclature follows ?Hmisc:::units.Surv
    # only change the 'time'/'start'/'stop' column, not the 'event' column !!
    nc <- dim(unclass(x))[2L]
    y[,-nc] <- as.double(asDifftime(asDifftime(x[,-nc], units = un), units = value))
  }
  at <- attr(y, which = 'inputAttributes', exact = TRUE)
  at$time$units <- value 
  if (length(at$time2$units)) at$time2$units <- value
  attr(y, which = 'inputAttributes') <- at
  return(y)
}


# base::`units<-`
#' @export
`units<-.Surv` <- function(x, value) {
  .Defunct(new = 'more_units<-.Surv')
}




#' @title Get Time from \link[survival]{Surv} Object
#' 
#' @description ..
#' 
#' @param x \link[survival]{Surv} objects
#' 
#' @param type \link[base]{character} scalar
#' 
#' @param ... potential parameters, currently not in use
#' 
#' @note 
#' Do not clash with S3 \link[stats]{time}!
#' 
#' @export
getTime.Surv <- function(x, type = c('event', 'censor', 'any'), ...) {
  evt <- as.logical(x[, 'status'])
  id <- switch(match.arg(type), any = TRUE, event = evt, censor = !evt)
  nc <- dim(x)[2L]
  if (nc == 3L) return((x[, 'stop'] - x[, 'start'])[id]) #Not sure if correct!!
  if (nc == 2L) return(x[, 'time'][id])
  stop('Surv object cannot have ncol>3L')
}





