#' Cook's statistics
#'
#' \code{cooks_stats} returns the ordered Cook's statistics
#' (distances) decreasing in value of \code{model} to identify the
#' most influential observations.
#' @inheritParams residual_plot.lm
#' @inheritParams utils::head
#' @seealso \code{\link[stats]{cooks.distance}}.
#'
#' @return A vector of statistics
#' @export
#'
#' @examples
#' lmod <- lm(price ~ sqft_living, data = home_sales)
#' cooks_stats(lmod, n = 5)
cooks_stats <- function(model, n = 6L) {
  if (!is.element("lm", class(model))) {
    stop("model must be an lm object")
  }
  cooks <- stats::cooks.distance(model)
  utils::head(cooks[order(abs(cooks), decreasing = TRUE)],
              n = n)
}