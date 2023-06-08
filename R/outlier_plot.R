#' Index plot of studentized residuals for \code{lm} object
#'
#' \code{outlier_plot} plots the studentized residuals (from
#' the \code{\link[stats]{rstudent}} function) of a fitted
#' \code{lm} object.
#'
#' If \code{add_reference = TRUE}, then reference lines are
#' provided for the \eqn{\alpha/(2n} and \eqn{1-\alpha/(2n)}
#' quantiles of a Student's \eqn{t} distribution with
#' (\code{df.residual(lmod) - 1}) degrees of freedom, which
#' are the standard quantiles used to identify outliers for
#' a fitted model.
#'
#' The vertical position of the reference line can be
#' customized by setting the \code{h} argument of
#' \code{abline_arglist}.
#'
#' @inheritParams residual_plot.lm
#' @param add_reference A logical value indicating whether a
#'   reference line should be added. The default is
#'   \code{TRUE}. See Details.
#' @param alpha The default lower quantile used for the
#'   reference line prior to the Bonferroni adjustment. The
#'   default is 0.05.
#' @author Joshua French
#' @seealso \code{\link[graphics]{plot}},
#'   \code{\link[graphics]{text}},
#'   \code{\link[graphics]{abline}},
#'   \code{\link[stats]{rstudent}}
#' @export
#' @examples
#' lmod <- lm(price ~ sqft_living, data = home_sales)
#' outlier_plot(lmod, id_n = 1)
outlier_plot <-
  function(model,
           id_n = 3,
           add_reference = TRUE,
           alpha = 0.05,
           ...,
           text_arglist = list(),
           abline_arglist = list(),
           extendrange_f = 0.08) {
  arglist <- list(...)

  arg_check_index_plot_lm(model, stats::rstudent, id_n,
                          add_reference = add_reference,
                          text_arglist = text_arglist,
                          abline_arglist = abline_arglist,
                          extendrange_f = extendrange_f)
  if (length(alpha) != 1) {
    stop("alpha must have length 1")
  }
  if (!is.numeric(alpha)) {
    stop("alpha must be a numeric vector")
  }
  if (min(alpha) <= 0 | max(alpha) >= 1) {
    stop("alpha must be in (0, 1)")
  }

  if(is.null(arglist$ylab)) {
    arglist$ylab <- "studentized residuals"
  }
  if (add_reference & is.null(abline_arglist$h)) {
    n <- stats::nobs(model)
    # loo residual degrees of freedom
    rdf <- stats::df.residual(model) - 1
    h <- c(-1, 1) * stats::qt(alpha/(2 * n), df = rdf)
    abline_arglist$h <- h
  }

  do.call(index_plot_lm,
          c(list(model = model,
                 stat = stats::rstudent,
                 id_n = id_n,
                 add_reference = add_reference,
                 text_arglist = text_arglist,
                 abline_arglist = abline_arglist,
                 extendrange_f = extendrange_f),
                 arglist)
  )
}
