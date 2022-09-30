#' Plot \code{confint_adjust} x
#'
#' Plot a \code{confint_adjust} x produced by the
#' \code{\link[api2lm]{confint_adjust}} function. See
#' Examples.
#'
#' The \code{plot} function doesn't automatically adjust the
#' margins to account for the label names. If you need more
#' space for your labels, then increase the second element
#' of \code{mar} from 7.1 upward and line upward.
#' Alternatively, if you need less space, then you can
#' decrease both of these values. Or you could use the
#' \code{autoplot} function that automatically controls the
#' spacing.
#'
#' @param x An \code{confint_adjust} x produced by the
#'   \code{\link[api2lm]{confint_adjust}} function.
#' @inheritParams stats::confint
#' @param mar A numerical vector of the form c(bottom, left,
#'   top, right) which gives the number of lines of margin
#'   to be specified on the four sides of the plot. The
#'   default is c(5, 7, 4, 2) + 0.1.
#' @param line The MARgin line, starting at 0 counting
#'   outwards, to draw the y-axis label. The default is 1
#'   unit less than \code{mar[2]}.
#' @param ... Additional arguments passed to \code{plot}.
#' @return None.
#' @author Joshua P. French
#' @export
#' @examples
#' fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
#' # standard intervals
#' cia <- confint_adjust(fit)
#' plot(cia)
#' # plot subset of intervals
#' plot(cia, parm = c("hp", "disp"))
#' # adjust margin and line for better formatting
#' plot(cia, parm = 2:3, mar = c(5.1, 4.1, 4.1, 2.1))
plot.confint_adjust = function(x,
                               parm,
                               mar = c(5.1, 7.1, 4.1, 2.1),
                               line = mar[2] - 1,
                               ...) {
  arglist <- list(...)
  ylab <- arglist$ylab
  if (is.null(ylab)) {
    ylab <- "term"
  }

  # select variables
  pnames <- x$term
  if (missing(parm)) {
    parm <- pnames
  } else if (is.numeric(parm)) {
    parm <- pnames[parm]
  }
  x <- x[is.element(x$term, parm),, drop = FALSE]

  x$number <- seq_len(nrow(x))
  # determine limits of x-axis
  xmin <- min(x$lwr)
  xmax <- max(x$upr)
  # setup plot
  term <- range(x$number)
  estimate <- c(xmin, xmax)
  # extract current margins
  cmar <- graphics::par()$mar
  # reset margin on exit
  base::on.exit(graphics::par(mar = cmar))
  # set new margins for plotting
  graphics::par(mar = mar)
  # create blank plot
  plot(term ~ estimate, yaxt = "n", type = "n",
       ylab = "", ...)
  # add axis tick labels
  graphics::axis(2, at = x$number, labels = x$term, las = 1)
  # add y-axis labels
  graphics::mtext(ylab, side = 2, line = line)
  # include segments for confidence intervals
  graphics::segments(
    x0 = x$lwr, y0 = x$number,
    x1 = x$upr, y1 = x$number)
  # add center of interval
  graphics::points(number ~ estimate, data = x)
  # vertical line at 0
  graphics::abline(v = 0, lty = 4)
}
