

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


#' @importFrom fastmd md_ md_.default
#' @export
md_.ggsurvplot <- md_.default # otherwise dispatch to ?fastmd::md_.list



