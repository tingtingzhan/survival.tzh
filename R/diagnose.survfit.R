
#' @importFrom survival survfit.formula
diagnose.Surv <- function(object, ...) {
  diagnose.survfit(survfit.formula(object ~ 1), ...)
}




#' @title diagnose.survfit
#' 
#' @description ..
#' 
#' @param object ..
#' 
#' @param type ..
#' 
#' @param ... ..
#' 
#' @references 
#' \url{http://www.ats.ucla.edu/stat/examples/asa/test_proportionality.htm}
#' 
#' @examples 
#' f1 = survfit(Surv(time, status == 2) ~ ph.ecog, data = lung)
#' #grid::grid.draw(diagnose(f1, type = 'survival'))
#' diagnose.survfit(f1, type = 'hazard')
#' @importFrom ggplot2 geom_ribbon geom_path labs scale_x_continuous scale_y_continuous
#' @importFrom gridExtra arrangeGrob
#' @importFrom scales percent
#' @export
diagnose.survfit <- function(object, type = 'survival', ...) {
  
  dat0 <- fortify.survfit(object)
  
  cat('See', sQuote('Survival Analysis Using S (2003)'), 'Chapter 3\n')
  
  # Proportional (if parallel) &\n  ??
  
  switch(type, survival = {
    
    p_loglog <- ggplot() + 
      autolayer.survfit(object, rm01 = TRUE, ...) +
      scale_x_continuous(trans = 'log') + 
      scale_y_continuous(trans = 'loglog') +
      labs(x = 'Time (with log-link)', y = 'Survival % (with loglog-link)', 
           title = 'Diagnose: Weibull/Expon. (if straight line)')
    
    p_logit <- ggplot() + 
      autolayer.survfit(object, rm01 = TRUE, ...) +
      scale_x_continuous(trans = 'log') +
      scale_y_continuous(trans = 'logit') +
      labs(y = 'Survival % (with logit-link)', 
           x = 'Time (with log-link)',
           title = 'Diagnose: log-logistic (if straight line)')

    return(arrangeGrob(grobs = list(p_loglog, p_logit)))
    
  }, hazard = {
    
    x <- as.integer(dat0$n.event)
    n <- as.integer(dat0$n.risk)
    #ci <- exact_confint(x = x, n = n, level = .95) # ?ThomasJeffersonUniv::exact_confint
    #colnames(ci) <- c('lower', 'upper')
    dat2 <- data.frame(values = x/n, time = dat0$time, 
                       #ci, 
                       strata = dat0$strata)
    
    ggplot() + 
      geom_path(mapping = aes(x = dat0$time, y = x/n, group = dat0$strata, colour = dat0$strata)) + 
      #geom_ribbon(mapping = aes(x = dat0$time, ymax = ci[,2L], ymin = ci[,1L], group = dat0$strata, fill = dat0$strata), alpha = .1) +
      scale_y_continuous(labels = percent) +
      labs(x = attr(object, which = 'units', exact = TRUE), y = 'Empirical Hazard', title = 'Diagnose: Exponential (expect constant)',
           fill = NULL, color = NULL)
  
  }, stop('illegal type: ', sQuote(type)))
  
}
