


#' @title Names of type in data (to be deprecated)
#' 
#' @description ..
#' 
#' @param x \link[base]{data.frame}
#' 
#' @details 
#' [vnumeric] ..
#' 
#' @name vars_in_data
#' @export
vnumeric <- function(x) names(x)[vapply(x, FUN = inherits, what = c(
  'integer', 'numeric', 'Date', 'difftime', 'unit', 'unit.list'
), FUN.VALUE = NA)]








maxEventTime <- function(x, units = units.Surv(x), ...) {
  if (all(is.na(x))) return('')
  if (!length(evtm <- getTime.Surv(x, type = 'event'))) return('No event time observed!!')
  trimws(paste('Max observed event time T* =', sprintf(max(evtm), fmt = '%.02f'), units))
} 





#' @title diagnose.coxph
#' 
#' @param object ..
#' 
#' @param type `'CoxSnell'` (default), 
#'   `'deviance'`, `'schoenfeld'`, `'dfbetas'`, 
#'   `'GrambschTherneau'`, `'martingale'`, `'dev_by_x'`
#' 
#' @param ... ..
#' 
#' @examples 
#' (c1 = coxph(Surv(time, status == 2) ~ age + sex + ph.ecog, data = lung))
#' diagnose.coxph(c1, type = 'GrambschTherneau')
#' diagnose.coxph(c1, type = 'CoxSnell')
#' #diagnose(c1, type = 'martingale') # debug this!!! could due to missing data
#' # the waiting time should enter the proportional hazards model nonlinearly
#' diagnose.coxph(c1, type = 'deviance') # mask for font
#' #diagnose(c1, type = 'dev_by_x') # debug this!!! could due to missing data
#' diagnose.coxph(c1, type = 'schoenfeld') # mask for font
#' # diagnose(c1, type = 'dfbetas') # ggscatter deprecated
#' @importFrom ggplot2 ggplot geom_point geom_smooth geom_abline labs scale_x_continuous
#' @importFrom scales percent
#' @importFrom survival Surv survfit.formula coxph cox.zph coxph.detail
#' @importFrom stats residuals fitted terms
#' @export
diagnose.coxph <- function(
  object, 
  type = c('GrambschTherneau', # new: log-hazards-ratio by time
           'CoxSnell', # new:
           'deviance', 'schoenfeld', 'dfbetas', 
           'martingale', 'dev_by_x'),
  ...) {
  
  type <- match.arg(type)
  
  res0 <- if (!any(type == c('GrambschTherneau'))) {
    tmp <- switch(type, CoxSnell = {
      object$y[,2L] - object$residuals
    }, residuals(object, type = switch(type, dev_by_x = 'deviance', type))) 
    if (!is.matrix(tmp)) tmp <- array(tmp, dim = c(length(tmp), 1L), dimnames = NULL)
    tmp
  }
  
  .zph <- cox.zph(object)

  if (!is.data.frame(data <- eval(object$call$data))) stop('`data` must be evaluable')
  .xnum <- intersect(attr(terms(object), which = 'term.labels', exact = TRUE), vnumeric(data))
  
  switch(type, martingale = , dev_by_x = {
    
    if (!length(.xnum)) {
      message('No continous covariate present')
      return(invisible())
    }

    switch(type, martingale = {
      .ylab <- 'Martingle'
      cat('\nFigure: Martingle Residuals (discretize continuous covariate if not smooth)\n\n')
    }, dev_by_x = {
      .ylab <- 'Deviance'
      cat('\nFigure: Deviance Residuals by Predictor (potential outliers)\n\n')
    })
    
    #plst <- lapply(.xnum, FUN = function(ix) {
      # now how to deal missingness?   `res0` has no-missingness, but `data` has
      #ggscatter.data.frame(
      #  na.omit(data.frame(values = res0, x = data[[ix]], Predictor = ix)), 
      #  ylab = paste(.ylab, 'Resid'), xlab = ix) 
    #})
    
    #return(arrangeGrob(grobs = plst))
    return(invisible())
    
  }, CoxSnell = {
    
    .cs <- survfit.formula(Surv(res0, object$y[,2L]) ~ 1, type = 'fleming-harrington')
    
    mp <- aes(y = -log(.cs$surv), x = .cs$time)
    ggplot() + 
      geom_point(mapping = mp, size = .7) + 
      geom_smooth(mapping = mp, method = 'loess', formula = y ~ x, linetype = 2L, size = .5) +
      geom_abline(mapping = aes(intercept = 0, slope = 1), colour = 'grey') +
      labs(x = 'Cox-Snell Residuals', y = 'Estimated Cumulative Hazard', title = 'Cumulative Hazard of Cox-Snell Residuals', subtitle = 'expect 45\u00B0 line')

  }, deviance = {# deviance residuals by *model*
    
    if (!length(.xnum)) {
      message('No continous covariate present')
      return(invisible())
    }
    
    ggplot() + 
      geom_point(mapping = aes(x = fitted(object), y = res0)) + # \code{survival:::fitted.coxph}
      labs(x = 'Risk Score (Linear Predictor)', y = 'Deviance Resid', 
           title = 'Diagnosis: Deviance Residuals', 
           subtitle = 'expect flat line')
    
  }, schoenfeld = {
    
    x0 <- if (FALSE) {# textbook does
      tmp <- coxph.detail(object)
      unname(tmp$y[tmp$y[,3L] == 1, 2L])
    } else .zph$x # I do
    
    if (!length(vnm <- dimnames(res0)[[2L]])) vnm <- 'x'
    n <- length(x0)
    mp <- aes(y = c(res0), x = rep(x0, times = length(vnm)), colour = rep(vnm, each = n), fill = rep(vnm, each = n))
    ggplot() + 
      geom_point(mapping = mp, size = .7) +
      geom_smooth(mapping = mp, formula = y ~ x, method = 'loess', linetype = 2L, size = .5) +
      scale_x_continuous(labels = percent) +
      labs(y = 'Schoenfeld Residuals', x = paste('Pencentile of', maxEventTime(object$y)),
           colour = NULL, fill = NULL,
           title = 'Diagnosis: Schoenfeld Residuals', 
           subtitle = 'outlying predictor values')

  }, dfbetas = {
    
    colnames(res0) <- names(object$coefficients) # dfbetas residuals do not provide coef name!
    
    ##ggplot() + 
    #  geom_point(mapping = aes(x = seq_along(res0), ))
    #ggscatter.data.frame(
    #  y = ggdata.matrix(y = res0), 
    #  o_pct = .02, # need to think about how to write ..
    #  ylab = 'Scaled Change in Coef (dfbetas)',
    #  xlab = 'Observations',
    #  main = 'Diagnosis: dfbetas (influential obs on estimated coef)'
    #)
    
  }, GrambschTherneau = {
    diagnose.cox.zph(.zph, orig_model = object, ...)
    
  })
  
}
  







# improves ?survival:::plot.cox.zph
# replacing the spline fit with lowess fit, and 
# plotting all predictors on a same figure
#' @importFrom scales percent
# @export
diagnose.cox.zph <- function(object, ..., orig_model) {
  
  .trans <- object$transform
  
  xlab <- switch(.trans, km = {
    if (!missing(orig_model)) {
      paste('Percentile of', maxEventTime(orig_model$y))
    } else 'Percentile of max observed event time'
  }, identity = {
    'Time'
  }, rank = {
    'survival Time by Rank'
  }, {
    stop('see ?coxph::cox.zph.  Need to understand more')
    #.trans_split <- strsplit(as.character(.trans), split = ' ')[[1L]] # 'factor' or 'character'
    #if (grepl(pattern = 'function\\(', x = .trans_split[1L])) {
    #  .ts0 <- .trans_split[-1L]
    #  if (!all(nzchar(.ts0))) stop('do not allow')
    #  paste(.ts0, collapse = ' ')
    #} else if (nchar(.trans) < 30) .trans else {
    #  'Customized Transformation of survival Time'
    #}
  })
  
  # stopifnot(is.matrix(object$y))
  vnm <- dimnames(object$y)[[2L]]
  n <- length(object$x)
  mp <- aes(x = rep(object$x, times = length(vnm)), y = c(object$y), colour = rep(vnm, each = n))
  ggplot() + 
    geom_point(mapping = mp, size = .7) + 
    geom_smooth(mapping = mp, method = 'loess', formula = y ~ x, linetype = 2L, size = .5) + 
    switch(.trans, km = scale_x_continuous(labels = percent)) +
    labs(x = xlab, y = 'Time Varying Coef: beta(t)', 
         colour = 'Predictors',
         title = 'Diagnosis: Grambsch Therneau Test (expect flat line)')

}






