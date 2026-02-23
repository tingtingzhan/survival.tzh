
#' @title Hour-Minute-Second
#' 
#' @description
#' The function \link[readxl]{read_excel} might read a date-less time stamp 
#' (e.g., `3:00:00 PM` as `1899-12-31 15:00:00 UTC`).
#' 
#' @param x a \link[base]{POSIXlt} or \link[base]{POSIXct} object
#' 
#' @param format see function \link[base]{as.POSIXlt.character}
#' 
#' @param ... ..
#' 
#' @returns
#' The function [hms()] returns a \link[base]{difftime} object.
#' 
#' @seealso 
#' \link[base]{ISOdatetime};
#' `lubridate:::year.default`; 
#' `lubridate:::tz.POSIXt`
#' 
#' @examples
#' f = tempfile(fileext = '.xlsx')
#' data.frame(
#'  id = c('A', 'B'),
#'  tm = c('8:30:00 AM', '3:00:00 PM')
#' ) |>
#'  writexl::write_xlsx(path = f)
#' 
#' x = f |>
#'  readxl::read_excel()
#' x$tm |> class() # 'character'
#' x$tm |> hms() |> suppressWarnings()
#' 
#' @keywords internal
#' @name hms
#' @export
hms <- function(x, ...) UseMethod(generic = 'hms')


#' @rdname hms
#' @export hms.character
#' @export
hms.character <- function(x, format = '%H:%M:%OS', ...) {
  x |> 
    as.POSIXlt.character(format = format, ...) |>
    hms.POSIXlt()
}



#' @rdname hms
#' @export hms.POSIXlt
#' @export
hms.POSIXlt <- function(x, ...) {
  
  if (!all(x$year == -1L, na.rm = TRUE) ||
      !all(x$mon == 11L, na.rm = TRUE) ||
      !all(x$mday == 31L, na.rm = TRUE)) warning('should not use [hms()]')
  
  # sapply(unclass(x), FUN = typeof)
  
  x0 <- x # start of time on that day
  
  int0 <- rep(0L, times = length(x))
  int0[is.na(x)] <- NA_integer_
  x0$hour <- x0$min <- int0
  
  dbl0 <- rep(0, times = length(x))
  dbl0[is.na(x)] <- NA_real_
  x0$sec <- dbl0
     
  difftime(time1 = x, time2 = x0, units = 'auto')
  
}


#' @rdname hms
#' @export hms.POSIXct
#' @export
hms.POSIXct <- function(x, ...) {
  x |> 
    as.POSIXlt.POSIXct() |> 
    hms.POSIXlt()
}
