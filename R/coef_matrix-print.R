#'
#' Print an object of class \code{coef_matrix} produced
#' by the \code{\link[api2lm]{coef_matrix}} function.
#'
#' @param x An \code{coef_matrix} object produced by the
#'   \code{\link[api2lm]{coef_matrix}} function.
#' @inheritParams base::print.data.frame
#' @param ... Additional arguments to the
#'   \code{\link[base]{print.data.frame}} function, such as
#'   \code{digits}.
#' @return NULL
#' @author Joshua French
#' @export
#' @return A \eqn{p \times 4} matrix with columns for the
#'   estimated coefficient, its standard error, t-statistic
#'   and corresponding (two-sided) p-value.
#' @examples
#' fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
#' (coefm <- coef_matrix(fit))
#' # print more digits
#' print(coefm, digits = 8)
print.coef_matrix = function(x, digits = 2, ...) {
  arglist = list(...)
  do.call(base::print.default,
          append(list(x = unclass(x),
                      digits = digits),
                      arglist))
}
