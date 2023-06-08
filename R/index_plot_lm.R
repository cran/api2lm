#' Index plot of statistics from of an \code{lm} object
#'
#' \code{index_plot_lm} creates an index plot of statistcs
#' from an \code{lm} object.
#'
#' @inheritParams residual_plot.lm
#' @param stat A function that can be applied to an \code{lm}
#' object and returns a vector of observations for each
#' observation used to fit the model.
#' @author Joshua French
#' @seealso \code{\link[graphics]{plot}},
#'   \code{\link[graphics]{text}},
#'   \code{\link[stats]{lm}},
#'   \code{\link[stats]{rstudent}},
#'   \code{\link[stats]{hatvalues}},
#'   \code{\link[stats]{cooks.distance}}
#' @export
#' @examples
#' lmod <- lm(Petal.Length ~ Sepal.Length + Species,
#'            data = iris)
#' # outlier plot
#' # number of observations
#' n <- stats::nobs(lmod)
#' # loo residual degrees of freedom
#' rdf <- stats::df.residual(lmod) - 1
#'
#' h <- c(-1, 1) * stats::qt(0.05/(2 * n), df = rdf)
#' index_plot_lm(lmod, stat = stats::rstudent,
#'               abline_arglist = list(h = h))
#'
#' # leverage plot
#' index_plot_lm(lmod, stat = stats::hatvalues, id_n = 1)
#' # Cook's distance
#' index_plot_lm(lmod, stat = stats::cooks.distance,
#'               id_n = 3)
index_plot_lm <-
  function(model,
           stat,
           id_n = 3,
           add_reference = FALSE,
           ...,
           text_arglist = list(),
           abline_arglist = list(),
           extendrange_f = 0.08) {
  curpar <- graphics::par(no.readonly = TRUE)
  arglist <- list(...)
  arg_check_index_plot_lm(model,
    stat, id_n, add_reference, text_arglist, abline_arglist,
    extendrange_f)

  # get elements for index plot
  index_elements <- index_plot_lm_elements(model, stat)
  x <- index_elements$x
  y <- index_elements$y
  labels <- index_elements$labs
  idd <- order(abs(y), decreasing = TRUE)[seq_len(id_n)]

  if (add_reference) {
    # set bonferroni-corrected reference level at 0.05
    if (is.null(abline_arglist$h)) {
      ref_text <-
        paste("If 'add_reference = TRUE' then the",
              "user must manually specify the 'h' argument",
              "of abline_arglist to indicate where",
              "the reference line(s) should be placed.")
      warning(ref_text)
    }
  }

  # get default name of y-axis statistic
  if (is.null(arglist$ylab)) {
    arglist$ylab <-
      utils::tail(as.character(substitute(stat)), 1)
  }
  index_plot_raw(x, y, idd, labels, add_reference,
                 arglist, text_arglist, abline_arglist,
                 extendrange_f)
  # proper exit
  on.exit(graphics::par(curpar))
}
