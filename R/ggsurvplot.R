

#' @title \link[survminer]{ggsurvplot}
#' 
#' @examples 
#' p = survfit(Surv(time, status) ~ sex, data = lung) |>
#'  survminer::ggsurvplot(data = lung)
#' list(
#'  '`ggsurvplot`' = p
#' ) |> fastmd::render2html(file = 'ggsurvplot')
#' 
#' @name ggsurvplot
NULL


#' @importFrom fastmd md_
#' @export
md_.ggsurvplot <- function(x, xnm, ...) {
  NextMethod(generic = 'md_')
  # otherwise dispatch to ?fastmd::md_.list
} 



