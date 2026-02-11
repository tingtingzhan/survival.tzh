
#' @title \link[utils]{bibentry} for Survival Analysis
#' 
#' @param key,... parameters of function \link[utils]{bibentry}
#' 
#' @keywords internal
#' @name survival_bib
#' @importFrom utils bibentry person
#' @export
.kaplan_meier58 <- function(key = 'KaplanMeier58', ...) {
  bibentry(
    bibtype = 'article', key = key, ...,
    author = c(
      person(given = c('Edward', 'L.'), family = 'Kaplan'), 
      person(given = 'Paul', family = 'Meier')
    ),
    title = 'Nonparametric Estimation from Incomplete Observations',
    journal = 'Journal of the American Statistical Association',
    volume = '53',
    number = '282',
    pages = '457--481',
    year = '1958',
    doi = '10.1080/01621459.1958.10501452'
  )
}


#' @rdname survival_bib
#' @export
.cox72 <- function(key = 'Cox72', ...) {
  bibentry(
    bibtype = 'Article', key = key, ...,
    author = person(given = c('David', 'R.'), family = 'Cox'),
    title = 'Regression Models and Life-Tables',
    journal = 'Journal of the Royal Statistical Society: Series B (Methodological)',
    volume = '34',
    number = '2',
    pages = '187-202',
    doi = '10.1111/j.2517-6161.1972.tb00899.x',
    year = '1972'
  )
}

#' @rdname survival_bib
#' @export
.ripatti04 <- function(key = 'Ripatti04', ...) {
  bibentry(
    bibtype = 'Article', key = key, ...,
    author = c(
      person(family = 'Ripatti', given = 'Samuli'), 
      person(family = 'Palmgren', given = 'Juni')
    ),
    title = 'Estimation of Multivariate Frailty Models Using Penalized Partial Likelihood',
    journal = 'Biometrics',
    volume = '56',
    number = '4',
    pages = '1016-1022',
    year = '2004',
    month = '05',
    doi = '10.1111/j.0006-341X.2000.01016.x'
  )
}




#' @rdname survival_bib
#' @export
.therneau03 <- function(key = 'Therneau03', ...) {
  bibentry(
    bibtype = 'Article', key = key, ...,
    author = c(
      person(given = c('Terry', 'M.'), family = 'Therneau'), 
      person(given = c('Patricia', 'M.'), family = 'Grambsch'), 
      person(given = c('V.', 'Shane'), family = 'Pankratz')
    ),
    title = 'Penalized Survival Models and Frailty',
    journal = 'Journal of Computational and Graphical Statistics',
    volume = '12',
    number = '1',
    pages = '156--175',
    year = '2003',
    doi = '10.1198/1061860031365'
  )
}