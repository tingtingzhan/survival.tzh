
#' @title Create Time Differences, Extended
#' 
#' @description
#' To create \link[base]{difftime} object 
#' with additional time units `'months'` and `'years'`.
#' 
#' @param tim \link[base]{numeric} or \link[base]{difftime} object, 
#' similar usage as in function \link[base]{as.difftime}
#' 
#' @param units \link[base]{character} scalar, 
#' similar usage as in function \link[base]{as.difftime},
#' but with additional options `'months'` and `'years'`
#' 
#' @param negative_do exception handling 
#' if input `tim` has negative element(s). 
#' Default is to \link[base]{stop}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details 
#' 
#' Function [asDifftime()] improves function \link[base]{as.difftime} in terms that
#' \itemize{
#' \item {If input `tim` is a \link[base]{difftime} object, 
#' function [more_units<-.difftime] is called and the unit of `tim` is updated.
#' In function \link[base]{as.difftime}, `tim` is returned directly, i.e., parameter `units` is ignored}
#' \item {Time units `'months'` and `'years'` are supported, 
#' in addition to `'secs'`, `'mins'`, `'hours'`, `'days'`, `'weeks'` supported in function \link[base]{as.difftime}.
#' Moreover, partial matching (via function \link[base]{match.arg}) is allowed,
#' while function \link[base]{as.difftime} requires exact matching.}
#' \item {End user may choose to \link[base]{stop} if `tim` has negative values.
#' Function \link[base]{as.difftime} does not check for negative `tim`.}
#' }
#' 
#' @returns 
#' Function [asDifftime()] returns a \link[base]{difftime} object.
#' 
#' @note 
#' Potential name clash with function \link[units]{as_difftime}.
#' 
#' @seealso
#' Function \link[lubridate]{time_length}, 
#' or `getMethod(f = lubridate::time_length, signature = 'Interval')`,
#' does not return a \link[base]{difftime} object.
#' 
#' @keywords internal
#' @export
asDifftime <- function(
    tim, 
    units = names(timeUnits()), 
    negative_do = stop(sQuote(deparse1(substitute(tim))), ' has negative value!'), 
    ...
) {
  
  if (any(id <- (unclass(tim) < 0), na.rm = TRUE)) {
    negative_do
  }
  
  unt <- match.arg(units)
  
  if (inherits(tim, what = 'difftime')) {
    more_units(tim) <- unt
    return(tim)
  }
  
  if (is.numeric(tim)) { # 'matrix' etc okay 
    return(difftime_int(tim, units = unt))
  }
  
  stop(sQuote(class(tim)[1L]), ' object cannot be converted to \'difftime\'')
  
}








#' @title Additional Time Units `'months'` and `'years'`
#' 
#' @description
#' To support additional time units `'months'` and `'years'` for \link[base]{difftime} object.
#' 
#' @details
#' Every 4 years has `1461(=365*4+1)` days, or `48(=4*12)` months.
#' Therefore, every month has `30.44(=1461/48)` days, or `4.35(=1461/48/7)` weeks.
#' 
#' Every year has 12 months.
#' 
#' 
#' @note
#' Function \link[base]{units<-.difftime} 
#' only supports 
#' `'secs'`, `'mins'`, `'hours'`, `'days'`, `'weeks'`.
#' 
#' @returns
#' Function [timeUnits()] returns a named constant \link[base]{character} \link[base]{vector}.
#' 
#' @keywords internal
#' @export
timeUnits <- function() c(
  secs = 1, mins = 60, hours = 60, days = 24, weeks = 7, # base::`units<-.difftime` 
  months = 1461/48/7, 
  years = 12
)







#' @title Use Month and Year as \link[base]{units}
#' 
#' @description 
#' Set \link[base]{units} of R objects,
#' with additional support of `'months'` and `'years'`.
#' 
#' @param x see **Usage**
#' 
#' @param value \link[base]{character} scalar, 
#' choice of unit, e.g., 
#' \itemize{
#' \item{`'secs'`, `'mins'`, `'hours'`, `'days'`, `'weeks'`,
#' which are also supported in the function \link[base]{units<-.difftime}}
#' \item{`'months'` and `'years'`, which are only supported by the function [more_units<-]}
#' }
#' 
#' @returns 
#' The `S3` method [more_units<-.difftime] returns a \link[base]{difftime} object.
#' 
#' @examples 
#' (x = Sys.Date() - as.Date('2021-01-01'))
#' tryCatch(units(x) <- 'months', error = identity)
#' more_units(x) = 'months'; x
#' more_units(x) = 'years'; x
#' 
#' @keywords internal
#' @name more_units_set
#' @export
`more_units<-` <- function(x, value) UseMethod(generic = 'more_units<-')

#' @rdname more_units_set
#' @export more_units<-.difftime
#' @export
# `units_difftime<-` 
`more_units<-.difftime` <- function(x, value = names(timeUnits())) {
  from <- attr(x, which = 'units', exact = TRUE)
  value <- match.arg(value)
  if (from == value) return(x)
  sc <- cumprod(timeUnits())
  newx <- unclass(x) * as.vector(sc[from]/sc[value])
  return(difftime_int(newx, units = value))
}




difftime_int <- \(x, units) {
  # ?base::.difftime do not enforce the *order* of attributes 'class' and 'units'.
  # This causes error in ?reshape2:::melt.data.frame which checks the exact order of attributes.
  # ?difftime_int will enforce the order of attributes as 'class', 'units', etc
  # which is the ?base::.difftime output of numeric input `x`
  atr <- attributes(x)
  old_cls <- unique.default(c(class(x), atr$class)) # `atr$class` and `class(x)` not all ways the same
  atr$class <- unique.default(c(setdiff(old_cls, c('numeric', 'integer')), 'difftime')) # keep c('matrix', 'array')
  atr$units <- units
  attributes(x) <- atr[unique.default(c('class', 'units', names(atr)))]
  return(x)
}




