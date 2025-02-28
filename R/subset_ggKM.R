

#' @title subset_ggKM
#' 
#' @description
#' 
#' Produce a batch of Kaplan-Meier curves for *subset analysis*.
#' 
#' @param formula \link[stats]{formula}
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param ids \link[base]{list} of \link[base]{integer} or \link[base]{logical} \link[base]{vector},
#' indices for subset of `data`
#' 
#' @param ... ..
#' 
#' @examples
#' dim(colon1 <- subset(colon, etype == 2)) # death
#' colon1a = within(colon1, expr = {
#'  time = as.difftime(time, units = 'days')
#'  OS = Surv(time, status)
#' })
#' ids = with(colon1a, list(All = TRUE, Female = (sex == 0), Male = (sex == 1)))
#' subset_ggKM(OS ~ rx, ids = ids, data = colon1a)
#' @importFrom ggplot2 labs layer_scales
#' @importFrom scales percent
#' @export
subset_ggKM <- function(formula, data, ids, ...) {
  
  nms <- names(ids)
  if (!length(nms) || anyNA(nms) || !all(nzchar(nms))) stop('`ids` must be fully named')
  
  p0 <- .mapply(FUN = function(id, nm) {
    idat <- data[id, , drop = FALSE]
    #eval(call(name = 'ggKM.formula', formula = formula, data = quote(idat), ...)) # error
    eval(as.call(list(quote(ggKM.formula), formula = formula, data = quote(idat), ...))) + labs(
      title = sprintf(fmt = '%s, n=%d', nm, .row_names_info(idat, type = 2L))
    )
  }, dots = list(id = ids, nm = nms), MoreArgs = NULL)
  
  ylim_ <- range.default(lapply(p0, FUN = function(p) layer_scales(p)$y$range$range))
  
  p <- lapply(p0, FUN = function(p) {
    #suppressMessages(p + scale_y_continuous(labels = percent, limits = ylim_))
    suppressMessages(p + scale_y_continuous(labels = percent, limits = ylim_))
  })
  
  attr(p, which = 'text') <- sprintf(fmt = 'Kaplan Meier curves %s on various subsets', sQuote(deparse1(formula)))
  class(p) <- c('subset_ggKM', class(p))
  return(p)

}


#' @importFrom gridExtra arrangeGrob
#' @importFrom grid grid.draw grid.newpage  
#' @export
print.subset_ggKM <- function(x, ncol = 2L, ...) {
  grid.newpage()
  grid.draw(arrangeGrob(grobs = x, ncol = ncol))
  return(invisible(x))
}

#' @title rmd_.subset_ggKM
#' 
#' @param x ..
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @export rmd_.subset_ggKM
#' @export
rmd_.subset_ggKM <- function(x, xnm, ...) {
  
  h <- attr(x, which = 'fig.height', exact = TRUE) %||% 4
  w <- attr(x, which = 'fig.width', exact = TRUE) %||% 7
  
  w <- 2 * w 
  h <- 4 * ceiling(length(x) / 2)
  
  return(c(
    sprintf(fmt = '```{r results = \'asis\', fig.height = %.1f, fig.width = %.1f}', h, w), 
    sprintf(fmt = 'suppressWarnings(print.subset_ggKM(%s, ncol = 2L))', xnm), 
    '```',
    '<any-text>'
  ))
  
}
  
  
  
  
