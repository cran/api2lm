#' Index plot of Cook's distances for \code{lm} object
#'
#' \code{cooks_plot} plots the Cook's distances from
#' the \code{\link[stats]{cooks.distance}} function of a fitted
#' \code{lm} object.
#'
#' By default, a reference line is plotted at the \code{0.5}
#' quantile of a \eqn{F_{p, n-p}} distribution
#' where \eqn{p = }\code{length(stats::coef(model))} and
#' \eqn{n - p = }\code{stats::df.residual(model)}.
#'
#' The vertical position of the reference line can be customized
#' by setting the \code{h} argument of \code{abline_arglist}.
#'
#' @inheritParams residual_plot.lm
#' @param add_reference A logical value indicating whether a
#'   reference line should be added. See details.

#' @author Joshua French
#' @seealso \code{\link[graphics]{plot}},
#'   \code{\link[graphics]{text}},
#'   \code{\link[graphics]{abline}},
#'   \code{\link[stats]{cooks.distance}}
#' @export
#' @examples
#' lmod <- lm(price ~ sqft_living, data = home_sales)
#' cooks_plot(lmod, id_n = 1)
cooks_plot <-
  function(model,
           id_n = 3,
           add_reference = TRUE,
           ...,
           text_arglist = list(),
           abline_arglist = list(),
           extendrange_f = 0.08) {
  arglist <- list(...)

  arg_check_index_plot_lm(model, stats::cooks.distance,
                          id_n,
                          add_reference = add_reference,
                          text_arglist = text_arglist,
                          abline_arglist = abline_arglist,
                          extendrange_f = extendrange_f)
  if(is.null(arglist$ylab)) {
    arglist$ylab <- "Cook's distance"
  }
  if (add_reference & is.null(abline_arglist$h)) {
    p <- length(stats::coef(model))
    rdf <- stats::df.residual(model)
    abline_arglist$h <- stats::qf(0.5, df1 = p, df2 = rdf)
  }

  do.call(index_plot_lm,
          c(list(model = model,
                 stat = stats::cooks.distance,
                 id_n = id_n,
                 add_reference = add_reference,
                 text_arglist = text_arglist,
                 abline_arglist = abline_arglist,
                 extendrange_f = extendrange_f),
                 arglist)
  )
}
