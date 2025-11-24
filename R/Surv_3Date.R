
#' @title Create \link[survival]{Surv} Object using Three \link[base]{Date}s
#' 
#' @description 
#' Create right-censored \link[survival]{Surv} object using start, stop and censoring dates.
#' 
#' @param start,stop,censor \link[base]{Date}, \link[base]{POSIXlt} or \link[base]{POSIXct} object
#' 
#' @param units (optional) \link[base]{character} scalar, time units
#' 
#' @param ... potential parameters, currently not in use
#' 
#' @returns 
#' Function [Surv_3Date()] returns a \link[survival]{Surv} object.
#' 
#' @examples 
#' d1 = within(survival::udca, expr = {
#'   edp_yr = Surv_3Date(entry.dt, death.dt, last.dt, units = 'years')
#'   edp_mon = Surv_3Date(entry.dt, death.dt, last.dt, units = 'months') 
#' })
#' head(d1)
#' 
#' noout = within(survival::udca, expr = {
#'   edp_bug = Surv_3Date(entry.dt, death.dt, as.Date('1991-01-01'), units = 'months') 
#' })
#' subset(survival::udca, subset = entry.dt > as.Date('1991-01-01')) # check error as suggested
#' 
#' @keywords internal
#' @importFrom survival Surv
#' @export
Surv_3Date <- function(
    start, stop, censor, 
    units = 'years',
    ...
) {
  
  start_nm <- substitute(start) |>
    deparse1(backtick = TRUE)
  stop_nm <- substitute(stop) |>
    deparse1(backtick = TRUE)
  censor_nm <- substitute(censor) |>
    deparse1(backtick = TRUE)
  
  start <- as.Date(start)
  stop <- as.Date(stop)
  censor <- as.Date(censor)
  
  stop2 <- stop - start # recycled; may have NA
  if (any(unclass(stop2) <= 0, na.rm = TRUE)) {
    sprintf(
      fmt = '\U0001f6d1 %s no-earlier than %s! See',
      'START' |> col_blue() |> style_bold(),
      'STOP' |> col_red() |> style_bold()
    ) |>
      message()
    sprintf(
      fmt = 'flextable.tzh::subset_(, subset = %s >= %s)',
      start_nm, stop_nm
      ) |>
      col_magenta() |> style_bold() |>
      message()
    return(invisible()) # dont stop; inspect multiple definition
  }
  
  censor2 <- censor - start # recycled; may have NA
  if (any(unclass(censor2) <= 0, na.rm = TRUE)) {
    sprintf(
      fmt = '\U0001f6d1 %s no-earlier than %s! See',
      'START' |> col_blue() |> style_bold(),
      'CENSOR' |> col_red() |> style_bold()
    ) |>
      message()
    sprintf(
      fmt = 'flextable.tzh::subset_(, subset = %s >= %s)',
      start_nm, censor_nm
    ) |>
      col_magenta() |> style_bold() |>
      message()
    return(invisible()) # dont stop; inspect multiple definition
  }

  units <- match.arg(units, choices = names(timeUnits()))
  units_difftime(stop2) <- units
  units_difftime(censor2) <- units
  
  censor3 <- pmax(stop2, censor2, na.rm = TRUE) 
  # some clinicians do not know we must have `cencor >= stop`
  # ?base::pmax (not ?base::pmax.int) can 
  # ... take care of 'difftime' input
  # ... recycles the length
  
  Surv(time = pmin(stop2, censor3, na.rm = TRUE), event = !is.na(stop2)) # beautiful!
  
}



