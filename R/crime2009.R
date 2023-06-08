#' @name crime2009
#' @title 2009 Crime Data
#' @description Data related to crime for the 50 U.S. states
#'   plus the District of Columbia. The data are taken from
#'   the \code{crime_data} data set available in the
#'   \code{statsmodels} package in Python. As stated in its
#'   documentation, "All data is for 2009 and was obtained
#'   from the American Statistical Abstracts except as
#'   indicated ...." The original documentation is available
#'   at
#'   \url{https://www.statsmodels.org/dev/datasets/generated/statecrime.html}.
#'
#'   The \code{violent} variable includes murder, forcible
#'   rape, robbery, and aggravated assault. Numbers for
#'   Illinois and Minnesota do not include forcible rapes.
#'   Footnote included with the American Statistical
#'   Abstract table reads: "The data collection methodology
#'   for the offense of forcible rape used by the Illinois
#'   and the Minnesota state Uniform Crime Reporting (UCR)
#'   Programs (with the exception of Rockford, Illinois, and
#'   Minneapolis and St. Paul, Minnesota) does not comply
#'   with national UCR guidelines. Consequently, their state
#'   figures for forcible rape and violent crime (of which
#'   forcible rape is a part) are not published in this
#'   table."
#'
#'   The \code{single} variable is calculated from 2009
#'   1-year American Community Survey obtained obtained from
#'   Census. Variable is Male householder, no wife present,
#'   family household combined with Female householder, no
#'   husband present, family household, divided by the total
#'   number of Family households.
#' @docType data
#' @usage data(crime2009)
#' @format A data frame with 51 observations and 7 variables:
#' \describe{
#'  \item{\code{violent}}{Rate of violent crimes per 100,000 persons in the population.}
#'  \item{\code{murder}}{Rate of murders per 100,000 persons in the population.}
#'  \item{\code{hs_grad}}{Percentage of the population having graduated from high school or higher.}
#'  \item{\code{poverty}}{Percentage of individuals with income below the poverty line.}
#'  \item{\code{white}}{Percentage of the population that is only considered "white" for race based on the 2009 American Community Survey.}
#'  \item{\code{single}}{Percentage of families made up of single individuals.}
#'  \item{\code{urban}}{Percentage of the population living in Urbanized Areas as of 2010 Census, where Urbanized Areas are areas of 50,000 or more people.}
#' }
#' @source A public domain data set available in the
#'   \code{statsmodels} python package. All data is for 2009
#'   and was obtained from the American Statistical
#'   Abstracts except as indicated.
#'   \url{https://www.statsmodels.org/dev/datasets/generated/statecrime.html}.
#' @keywords data
NULL
