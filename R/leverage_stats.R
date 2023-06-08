#' Leverage statistics
#'
#' \code{leverage_stats} returns the ordered
#' leverage values (decreasing) of
#' \code{model} to
#' identify the highest leverage observations.
#' @inheritParams residual_plot.lm
#' @inheritParams utils::head
#'
#' @return A vector of statistics
#' @export
#'
#' @examples
#' lmod <- lm(price ~ sqft_living, data = home_sales)
#' leverage_stats(lmod, n = 4)
leverage_stats <- function(model, n = 6L) {
  if (!is.element("lm", class(model))) {
    stop("model must be an lm object")
  }
  h <- stats::hatvalues(model)
  utils::head(h[order(abs(h), decreasing = TRUE)], n = n)
}