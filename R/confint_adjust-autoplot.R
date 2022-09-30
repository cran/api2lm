#' Plot \code{confint_adjust} object
#'
#' Plot a \code{confint_adjust} object produced by the
#' \code{\link[api2lm]{confint_adjust}} function. The plotting
#' function internally calls the
#' \code{\link[ggplot2]{autoplot}} function. Note: the
#' \code{ggplot2} package must be loaded (i.e.,
#' \code{library(ggplot2)} or \code{ggplot2::autoplot}
#' must be specifically called for this function to work.
#' See Examples.
#'
#' @param object An \code{confint_adjust} object produced by the
#'   \code{\link[api2lm]{confint_adjust}} function.
#' @inheritParams stats::confint
#' @param ... Not used
#' @return NULL
#' @author Joshua French
#' @return None.
#' @export
#' @examples
#' fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
#' # standard intervals
#' cia <- confint_adjust(fit)
#' # if ggplot2 package is available
#' if (require(ggplot2)) {
#' autoplot(cia)
#' # select subset of plots
#' autoplot(cia, parm = c("hp", "disp"))
#' }
autoplot.confint_adjust = function(object, parm, ...) {
  if (!requireNamespace("ggplot2")) {
    stop("ggplot2 must be installed to enable this functionality")
  }
  # select variables
  pnames <- object$term
  if (missing(parm)) {
    parm <- pnames
  } else if (is.numeric(parm)) {
    parm <- pnames[parm]
  }
  object <- object[is.element(object$term, parm),, drop = FALSE]

  # hack to stop warning from  CRAN about global variables
  # term <- lwr <- upr <- estimate <- NULL
  # the aes_(y = ~term) is to correct a warning from CRAN
  # about global variables. Normally, you could use
  # aes(y = ~term). This fix is repeated multiple times.
  ggplot2::ggplot(
    data = object,
    mapping = ggplot2::aes_(y = ~term)
    ) +
  ggplot2::geom_errorbarh(
    mapping = ggplot2::aes_(
      xmin = ~lwr,
      xmax = ~upr)
    ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes_(x = ~estimate)
    ) +
  ggplot2::geom_vline(xintercept = 0, lty = 4)
}
