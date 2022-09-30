#' Print \code{confint_adjust} object
#'
#' Print an object of class \code{confint_adjust} produced
#' by the \code{\link[api2lm]{confint_adjust}} function.
#'
#' @param x An \code{confint_adjust} object produced by the
#'   \code{\link[api2lm]{confint_adjust}} function.
#' @param ... Additional arguments to the
#'   \code{\link[base]{print.data.frame}} function, such as
#'   \code{digits}.
#' @return NULL
#' @author Joshua French
#' @return A \code{data.frame} with columns \code{term},
#'   \code{lwr}, and \code{upr}, which are the coefficients
#'   for which inference is being made, and the lower and
#'   upper bounds of the confidence intervals for each
#'   coefficient, respectively.
#' @export
#' @examples
#' fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
#' (cia <- confint_adjust(fit))
#' print(cia, digits = 3)
print.confint_adjust = function(x, ...) {
  method <- attr(x, "method")
  if (method == "none") {
    cat("\nUnadjusted confidence intervals\n")
  } else if (method == "bonferroni") {
    cat("\nBonferroni-adjusted confidence intervals\n")
  } else if (method == "wh") {
    cat("\nWorking-Hotelling-adjusted confidence intervals\n")
  }
  cat(paste("\nFamily-wise confidence level of at least",
            attr(x, "adj_level")),"\n\n")
  print.data.frame(x[c("term", "lwr", "upr")], row.names = FALSE, ...)
}
