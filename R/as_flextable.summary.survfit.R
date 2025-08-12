


#' @title as_flextable.summary.survfit
#' 
#' @param x returned object from function `survival:::summary.survfit`
#' 
#' @param which ..
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom flextable as_flextable add_header_lines set_caption
#' @importFrom flextable.tzh as_flextable.matrix
#' @importFrom reshape2 acast dcast
#' @importFrom scales label_percent
#' @export as_flextable.summary.survfit
#' @export
as_flextable.summary.survfit <- function(
    x, 
    which = c('n.risk', 'surv', 'surv.confint'), 
    ...
) {
  
  which <- match.arg(which)
  
  if (which == 'surv.confint') {
    x$surv.confint <- sprintf(fmt = '%.1f%%\n(%.1f%% - %.1f%%)',1e2*x$surv, 1e2*x$lower, 1e2*x$upper)
  }
  
  z <- if (length(x$strata)) {# with strata
    x[c('strata', which, 'time')] |>
      as.data.frame.list() |>
      acast(formula = strata ~ time, value.var = which)
  } else {# without strata
    x[c(which, 'time')] |>
      as.data.frame.list() |> 
      acast(formula = . ~ time, value.var = which)
  }
  
  names(dimnames(z)) <- c('Strata', 'Time')
  
  if (which == 'surv') {
    z[] <- z |> 
      label_percent(accuracy = .1)()
  }
  
  table_title <- switch(which, n.risk = {
    'Number at Risk'
  }, surv = {
    'Percentage Survived'
  }, surv.confint = {
    sprintf(fmt = 'Percentage Survived (%.0f%% CI)', 1e2*x$conf.int)
  })
  
  z |> 
    as_flextable.matrix() |>
    # set_caption(caption = table_title) # does not work in rmarkdown..
    add_header_lines(values = table_title)
  
}
