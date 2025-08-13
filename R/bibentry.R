
#' @title \link[utils]{bibentry} for Survival Analysis
#' 
#' @name survival_bibentry
#' @importFrom utils bibentry
#' @export
KaplanMeier58 <- function() {
  bibentry(
    bibtype = 'article', key = 'KaplanMeier58',
    author = c('Edward L. Kaplan', 'Paul Meier'),
    title = 'Nonparametric Estimation from Incomplete Observations',
    journal = 'Journal of the American Statistical Association',
    volume = '53',
    number = '282',
    pages = '457--481',
    year = '1958',
    doi = '10.1080/01621459.1958.10501452'
  )
}


#' @rdname survival_bibentry
#' @export
Cox72 <- function() {
  bibentry(
    bibtype = 'Article', key = 'Cox72',
    author = 'David R. Cox',
    title = 'Regression Models and Life-Tables',
    journal = 'Journal of the Royal Statistical Society: Series B (Methodological)',
    volume = '34',
    number = '2',
    pages = '187-202',
    doi = '10.1111/j.2517-6161.1972.tb00899.x',
    year = '1972'
  )
}

#' @rdname survival_bibentry
#' @export
Ripatti04 <- function() {
  bibentry(
    bibtype = 'Article', key = 'Ripatti04',
    author = 'Ripatti, Samuli and Palmgren, Juni',
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




#' @rdname survival_bibentry
#' @export
Therneau03 <- function() {
  bibentry(
    bibtype = 'Article', key = 'Therneau03',
    author = 'Terry M Therneau and Patricia M Grambsch and V. Shane Pankratz',
    title = 'Penalized Survival Models and Frailty',
    journal = 'Journal of Computational and Graphical Statistics',
    volume = '12',
    number = '1',
    pages = '156--175',
    year = '2003',
    doi = '10.1198/1061860031365'
  )
}