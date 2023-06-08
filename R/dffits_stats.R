#' DFFITS statistics
#'
#' \code{dffits_stats} returns the ordered DFFITS values
#' (decreasing in magnitude) of \code{model} to identify the
#' observations with the highest DFFITS values.
#' @inheritParams residual_plot.lm
#' @inheritParams utils::head
#' @seealso \code{\link[stats]{dffits}}
#' @return A vector of statistics
#' @export
#'
#' @examples
#' lmod <- lm(price ~ sqft_living, data = home_sales)
#' dffits_stats(lmod, n = 5)
dffits_stats <- function(model, n = 6L) {
  if (!is.element("lm", class(model))) {
    stop("model must be an lm object")
  }
  dffits <- stats::dffits(model)
  utils::head(dffits[order(abs(dffits), decreasing = TRUE)],
              n = n)
}