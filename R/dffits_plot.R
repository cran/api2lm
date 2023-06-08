#' Index plot of DFFITS values for \code{lm} object
#'
#' \code{dffits_plot} plots the DFFITS values from
#' the \code{\link[stats]{dffits}} function of a fitted
#' \code{lm} object.
#'
#' By default, a reference line is plotted at \eqn{\pm 2\sqrt{p/n}},
#' where \eqn{p = }\code{length(stats::coef(model))} and
#' \eqn{n = }\code{stats::nobs(model)}. This can be customized
#' by setting the \code{h} argument of \code{abline_arglist}.
#'
#' @inheritParams residual_plot.lm
#' @param add_reference A logical value indicating whether a
#'   reference line should be added. The default is
#'   \code{TRUE}. See Details.
#' @author Joshua French
#' @seealso \code{\link[graphics]{plot}},
#'   \code{\link[graphics]{text}},
#'   \code{\link[graphics]{abline}},
#'   \code{\link[stats]{dffits}}
#' @export
#' @examples
#' lmod <- lm(price ~ sqft_living, data = home_sales)
#' dffits_plot(lmod, id_n = 6)
#' # customized plot
#' dffits_plot(lmod, id_n = 1,
#'             text_arglist = list(col = "blue", cex = 2),
#'             abline_arglist = list(col = "red", lwd = 2))
dffits_plot <-
  function(model,
           id_n = 3,
           add_reference = TRUE,
           ...,
           text_arglist = list(),
           abline_arglist = list(),
           extendrange_f = 0.08) {
  arglist <- list(...)
  arg_check_index_plot_lm(model, stats::dffits, id_n,
                          add_reference = add_reference,
                          text_arglist = text_arglist,
                          abline_arglist = abline_arglist,
                          extendrange_f = extendrange_f)

  # set some arguments
  if(is.null(arglist$ylab)) {
    arglist$ylab <- "DFFITS"
  }
  if (add_reference & is.null(abline_arglist$h)) {
    p <- length(stats::coef(model))
    n <- stats::nobs(model)
    abline_arglist$h <- c(-1, 1) * 2 * sqrt(p/n)
  }

  do.call(index_plot_lm,
          c(list(model = model,
                 stat = stats::dffits,
                 id_n = id_n,
                 add_reference = add_reference,
                 text_arglist = text_arglist,
                 abline_arglist = abline_arglist,
                 extendrange_f = extendrange_f),
                 arglist)
  )
}
