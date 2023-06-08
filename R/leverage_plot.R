#' Index plot of leverage values for \code{lm} object
#'
#' \code{leverage_plot} plots the leverage (hat) values from
#' the \code{\link[stats]{hatvalues}} function of a fitted
#' \code{lm} object.
#'
#' @inherit leverage_test details
#' @inheritParams residual_plot.lm
#' @param add_reference A logical value indicating whether a
#'   reference line should be added. The default is
#'   \code{TRUE}.
#' @inheritParams leverage_test
#' @author Joshua French
#' @seealso \code{\link[graphics]{plot}},
#'   \code{\link[graphics]{text}},
#'   \code{\link[graphics]{abline}},
#'   \code{\link[stats]{rstudent}}
#' @export
#' @examples
#' lmod <- lm(price ~ sqft_living, data = home_sales)
#' # reference line not visible on plot because all
#' # leverage values are less than 0.5
#' leverage_plot(lmod, id_n = 2)
#' # different reference line
#' leverage_plot(lmod, id_n = 6, ttype = "2mean")
#' # custom reference line
#' leverage_plot(lmod, id_n = 2, ttype = "custom",
#'               threshold = 0.15)
leverage_plot <-
  function(model,
           id_n = 3,
           add_reference = TRUE,
           ttype = "half",
           threshold = NULL,
           ...,
           text_arglist = list(),
           abline_arglist = list(),
           extendrange_f = 0.08) {
  arglist <- list(...)
  ttype <- match.arg(ttype, c("half", "2mean", "custom"))
  threshold <- arg_check_leverage_test(model, 1, ttype,
                                       threshold)
  arg_check_index_plot_lm(model, stats::hatvalues, id_n,
                          add_reference = add_reference,
                          text_arglist = text_arglist,
                          abline_arglist = abline_arglist,
                          extendrange_f = extendrange_f)

  # set some arguments
  if(is.null(arglist$ylab)) {
    arglist$ylab <- "leverage"
  }
  if (add_reference & is.null(abline_arglist$h)) {
    abline_arglist$h <- threshold
  }

  do.call(index_plot_lm,
          c(list(model = model,
                 stat = stats::hatvalues,
                 id_n = id_n,
                 add_reference = add_reference,
                 text_arglist = text_arglist,
                 abline_arglist = abline_arglist,
                 extendrange_f = extendrange_f),
                 arglist)
  )
}
