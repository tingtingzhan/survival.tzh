
# Returned value of ?survival::survreg

#' @export
.pval.survreg <- function(x) {
  xsum <- summary(x) # ?survival:::summary.survreg
  ret <- xsum$table[, 'p']
  names(ret) <- rownames(xsum$table)
  return(ret)
}





