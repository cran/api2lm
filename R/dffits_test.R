#' Identify influential observations
#'
#' \code{dffits_test} returns the observations identified
#' as influential based on the absolute value of the DFFITS statistics being
#' larger than a threshold. \cr\cr The threshold used is \eqn{2\sqrt{p/n}},
#' where \eqn{p = }\code{length(stats::coef(model))} and
#' \eqn{n = }\code{stats::nobs(model)}.
#' @inheritParams residual_plot.lm
#' @param n The number of outliers to return. The default is
#'   all influential observations.
#'
#' @return A vector of influential observations.
#' @export
#'
#' @seealso \code{\link[stats]{dffits}}
#' @examples
#' lmod <- lm(price ~ sqft_living, data = home_sales)
#' dffits_test(lmod)
dffits_test <-
  function(model,
           n = stats::nobs(model)) {
  arg_check_dffits_test(model, n)

  p <- length(stats::coef(model))
  n <- stats::nobs(model)
  threshold <- 2 * sqrt(p/n)

  d <- stats::dffits(model)
  d <- d[order(abs(d), decreasing = TRUE)]

  d <- d[abs(d) > threshold]
  if(length(d) == 0) {
    cat(paste("No influential observations detected using a ",
              "threshold of +/-", threshold, ".\n", sep = ""))
    return(invisible(d))
  } else {
    if (length(d) > n) {
      return(utils::head(d, n = n))
    } else {
      return(d)
    }
  }
}

#' Check arguments of \code{dffits_test}
#'
#' @inheritParams dffits_test
#' @keywords internal
arg_check_dffits_test <- function(model, n) {
  if (!is.element("lm", class(model))) {
    stop("model must be an lm object")
  }
  if (length(n) != 1 | !is.numeric(n)) {
    stop("n must be a single number")
  }
  if (n < 1) {
    stop("n must be 1 or more")
  }
}