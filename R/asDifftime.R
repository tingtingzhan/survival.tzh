
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
#' function [units_difftime<-] is called and the unit of `tim` is updated.
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
#' Potential name clash with function \link[units]{as_difftime}
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
    units_difftime(tim) <- unt
    return(tim)
  }
  
  if (is.numeric(tim)) { # 'matrix' etc okay 
    return(difftime_int(tim, units = unt))
  }
  
  stop(sQuote(class(tim)[1L]), ' object cannot be converted to \'difftime\'')
  
}


