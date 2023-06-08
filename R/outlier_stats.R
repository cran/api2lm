#' Outlier statistics
#'
#' \code{outlier_stats} returns the ordered
#' studentized residuals (decreasing based on magnitude) of
#' \code{model} to
#' Identify the most unusual observations.
#' @inheritParams residual_plot.lm
#' @inheritParams utils::head
#'
#' @return A vector of statistics.
#' @export
#'
#' @examples
#' lmod <- lm(price ~ sqft_living, data = home_sales)
#' outlier_stats(lmod, n = 3)
outlier_stats <- function(model, n = 6L) {
  if (!is.element("lm", class(model))) {
    stop("model must be an lm object")
  }
  r <- stats::rstudent(model)
  utils::head(r[order(abs(r), decreasing = TRUE)], n = n)
}