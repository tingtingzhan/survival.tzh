#' @title Convert No-Date \link[base]{POSIXct} to \link[base]{difftime}
#' 
#' @description
#' ..
#' 
#' @param x \link[base]{POSIXct} 
#' 
#' @param else_return exception handling
#' 
#' @details
#' The function \link[readxl]{read_excel} reads `'hour:min:sec'` as `'1899-12-31 hr:min:sec UTC'`
#' 
#' @note
#' The function \link[base]{as.difftime} is **not** an S3 generic function.
#' 
#' @seealso 
#' `lubridate:::year.default`; 
#' `lubridate:::tz.POSIXt`
#' @keywords internal
#' @export
POSIXct2difftime <- function(
    x, 
    else_return = stop('Expecting all \'1899-12-31 hr:min:sec UTC\'')
) {
  tz <- attr(x, which = 'tzone', exact = TRUE)
  if (!length(tz)) stop('really?')
  if (length(tz) > 1L) stop('really?')
  yr <- as.POSIXlt.POSIXct(x, tz = tz)$year
  if (!all(yr == -1, na.rm = TRUE)) return(else_return)
  x - as.POSIXct(strptime('1899-12-31 00:00:00', format = '%Y-%m-%d %H:%M:%S'), tz = tz)
}