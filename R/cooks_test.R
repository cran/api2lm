#' Identify influential observations
#'
#' \code{cooks_test} returns the observations identified
#' as influential based on the cooks statistics being
#' larger than a threshold. \cr\cr The threshold for this
#' test is the \code{0.5}
#' quantile of a \eqn{F_{p, n-p}} distribution
#' where \eqn{p = }\code{length(stats::coef(model))} and
#' \eqn{n - p = }\code{stats::df.residual(model)}.
#' @inheritParams residual_plot.lm
#' @param n The number of outliers to return. The default is
#'   all influential observations.
#'
#' @return A vector of influential observations.
#' @export
#'
#' @seealso \code{\link[stats]{cooks.distance}}
#' @examples
#' lmod <- lm(price ~ sqft_living, data = home_sales)
#' cooks_test(lmod)
cooks_test <-
  function(model,
           n = stats::nobs(model)) {
  arg_check_cooks_test(model, n)

  p <- length(stats::coef(model))
  dfr <- stats::df.residual(model)
  threshold <- stats::qf(0.5, df1 = p, df2 = dfr)

  d <- stats::cooks.distance(model)
  d <- d[order(abs(d), decreasing = TRUE)]

  d <- d[abs(d) > threshold]
  if(length(d) == 0) {
    cat(paste("No influential observations detected using a ",
              "threshold of ", threshold, ".\n", sep = ""))
    return(invisible(d))
  } else {
    if (length(d) > n) {
      return(utils::head(d, n = n))
    } else {
      return(d)
    }
  }
}

#' Check arguments of \code{cooks_test}
#'
#' @inheritParams cooks_test
#' @keywords internal
arg_check_cooks_test <- function(model, n) {
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