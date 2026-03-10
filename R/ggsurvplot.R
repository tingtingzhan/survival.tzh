

#' @title \link[survminer]{ggsurvplot}
#' 
#' @examples 
#' p = survfit(Surv(time, status) ~ sex, data = lung) |>
#'  survminer::ggsurvplot(data = lung)
#' list(
#'  '`ggsurvplot`' = p
#' ) |> fastmd::render2html()
#' 
#' @name ggsurvplot
NULL


#' @export
md_.ggsurvplot <- function(x, ...) {
  md_int(x, engine = 'print', ...) # survminer:::print.ggsurvplot
} # otherwise dispatch to ?fastmd::md_.list



