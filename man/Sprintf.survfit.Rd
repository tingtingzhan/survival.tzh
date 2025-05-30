% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survfit.R
\name{Sprintf.survfit}
\alias{Sprintf.survfit}
\title{Sprintf.survfit}
\usage{
Sprintf.survfit(x)
}
\arguments{
\item{x}{\link[survival]{survfit.object}}
}
\description{
..
}
\examples{
library(survival)
survfit(Surv(time, status) ~ 1, data = aml) |> Sprintf.survfit()
survfit(Surv(time, status) ~ x, data = aml) |> Sprintf.survfit()
}
