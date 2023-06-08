#' Identify leverage points
#'
#' \code{leverage_test} returns the observations identified
#' as a leverage point based on a threshold.
#'
#' If \code{ttype = "half"}, the threshold is \code{0.5}.
#'
#' If \code{ttype = "2mean"}, the threshold is \eqn{2p/n},
#' where \eqn{p = }\code{length(stats::coef(model))} and
#' \eqn{n = }\code{stats::nobs(model)}, which is double the
#' mean leverage value.
#'
#' If \code{ttype = "custom"} then the user must manually
#' specify \code{threshold}.
#'
#' @inheritParams residual_plot.lm
#' @param n The number of leverage points to return. The
#'   default is all leverage points.
#' @param ttype Threshold type. The default is
#'   \code{"half"}. The other options are \code{"2mean"} and
#'   \code{"custom"}. See Details.
#' @param threshold A number between 0 and 1. Any
#'   observation with a leverage value above this number is
#'   declared a leverage point. This is automatically
#'   determined unless \code{ttype = "custom"}.
#'
#' @return A vector of statistics
#' @export
#' @seealso \code{\link[stats]{hatvalues}}
#' @examples
#' lmod <- lm(price ~ sqft_living, data = home_sales)
#' # comparison of results using different threshold types
#' leverage_test(lmod)
#' leverage_test(lmod, ttype = "2mean", n = 7)
#' leverage_test(lmod, ttype = "custom", threshold = 0.1)
leverage_test <-
  function(model, n = stats::nobs(model), ttype = "half",
           threshold = NULL) {
  ttype <- match.arg(ttype, c("half", "2mean", "custom"))
  threshold <- arg_check_leverage_test(model, n, ttype,
                                       threshold)
  h <- stats::hatvalues(model)
  h <- h[order(h, decreasing = TRUE)]
  h <- h[h > threshold]
  if(length(h) == 0) {
    cat(paste("No leverage points detected using a ",
              "threshold of ", threshold, ".\n", sep = ""))
    return(invisible(h))
  } else {
    if (length(h) > n) {
      return(utils::head(h, n = n))
    } else {
      return(h)
    }
  }
}

#' Check arguments of \code{leverage_test}
#'
#' @inheritParams leverage_test
#' @return A vector of statistics
arg_check_leverage_test <-
  function(model, n, ttype, threshold) {
  if (!is.element("lm", class(model))) {
    stop("model must be an lm object")
  }
  if (length(n) != 1 | !is.numeric(n)) {
    stop("n must be a single number")
  }
  if (n < 1) {
    stop("n must be 1 or more")
  }
  if (length(ttype) != 1 | !is.character(ttype)) {
    stop("ttype must be a single character string")
  }
  if (!is.element(ttype, c("half", "2mean", "custom"))) {
    stop("invalid ttype")
  }
  if (ttype == "custom" & is.null(threshold)) {
    stop("threshold must be specififed when ttype is 'custom'")
  }
  if (ttype == "half") {
    threshold <- 0.5
  } else if (ttype == "2mean") {
    p <- length(stats::coef(model))
    n <- stats::nobs(model)
    threshold <- 2 * p / n
  }
  if (length(threshold) != 1 || !is.numeric(threshold)) {
    stop("threshold must be a single value")
  }
  if (threshold <= 0 | threshold >= 1) {
    stop("threshold must be in (0, 1)")
  }
  return(threshold)
}