#' Spread-level plot for \code{lm} object
#'
#' \code{sl_plot.lm} plots a spread-level plot of a fitted
#' \code{lm} object. In general, it is intended to provide
#' similar functionality to \code{\link[stats]{plot.lm}}
#' when \code{which = 3}, but can be used for different
#' types of residuals and can also plot first-order
#' predictor variables along the x-axis instead of only the
#' fitted values.\cr\cr Details about the different types of
#' residuals are discussed in the
#' \code{\link[api2lm]{get_residuals}} function.
#'
#' @param model A fitted model object from the
#'   \code{\link[stats]{lm}} function.
#' @param rtype The residual type to plot. The default is
#'   \code{"ordinary"}. The other options are
#'   \code{"standardized"}, \code{"studentized"},
#'   \code{"loo"}, \code{"jackknife"}, \code{"deleted"},
#'   \code{"internally studentized"}, \code{"externally
#'   studentized"}.
#' @param xaxis The variable to use on the x-axis of the
#'   plot(s). The default is \code{"fitted"} to use fitted
#'   values. The other option is \code{"predictors"}.
#' @param id_n The number of points to identify with labels.
#'   The default is \code{3}.
#' @param add_smooth A logical value indicating whether a
#'   smooth should be added to each plot produced. The
#'   default is \code{TRUE}.
#' @param predictors A formula describing the first-order
#'   predictors to plot the residuals against. The default
#'   is all available first-order predictors.
#' @param smooth A function with a
#'   \code{\link[stats]{formula}} argument to smooth the
#'   desired plot. The default function is
#'   \code{\link[stats]{loess}.}
#' @param ... Additional arguments passed to the
#'   \code{\link[graphics]{plot}} function.
#' @param text_arglist Additional arguments passed to the
#'   \code{\link[graphics]{text}} function, which is used to
#'   display the points that are identified.
#' @param smooth_arglist A named list specifying additional
#'   arguments passed to the function provided in the
#'   \code{smooth} argument.
#' @param lines_arglist A named list specifying additional
#'   arguments passed to the \code{\link[graphics]{lines}}
#'   function for plotting the result of applying the
#'   \code{smooth} function.
#' @author Joshua French
#' @seealso \code{\link[graphics]{plot}},
#'   \code{\link[graphics]{text}},
#'   \code{\link[graphics]{lines}}
#'   \code{\link[stats]{loess}}.
#' @export
#' @examples
#' lmod <- lm(Petal.Length ~ Sepal.Length + Species,
#'            data = iris)
#' # similarity with built-in plot.lm functionality
#' sl_plot(lmod)
#' plot(lmod, which = 3)
#' # spread-level plot for other residual types
#' sl_plot(lmod, rtype = "studentized", id_n = 0)
#' # spread-level plot for predictors
#' sl_plot(lmod, xaxis = "pred", id_n = 2)
#' # spread-level plot for individual predictors
#' sl_plot(lmod, xaxis = "pred",
#'         predictors = ~ Sepal.Length,
#'         id_n = 2)
sl_plot.lm <-
  function(model,
           rtype = c("standardized",
                     "studentized",
                     "internally studentized",
                     "externally studentized"),
           xaxis = "fitted",
           id_n = 3, predictors = ~ .,
           smooth = stats::loess,
           add_smooth = TRUE,
           ...,
           text_arglist = list(),
           smooth_arglist = list(),
           lines_arglist = list()) {
  arglist <- list(...)
  rtype <- match.arg(rtype, c("standardized",
                             "studentized",
                             "internally studentized",
                             "externally studentized"))

  arg_check_residual_plot_lm(rtype, xaxis, id_n, smooth,
                             FALSE, add_smooth,
                             text_arglist, list(),
                             smooth_arglist, lines_arglist,
                             extendrange_f = 0)

  if(is.null(arglist$ylab)) {
    rtext <- paste(rtype, "residuals")
    # taken from plot.lm
    yl <- as.expression(substitute(sqrt(abs(YL)),
                                   list(YL = as.name(rtext))))
    arglist$ylab <- yl
  }

  # get desired residuals
  r <- sqrt(abs(get_residuals(model, rtype)))
  # determine id_n most extreme observations
  idd <- order(abs(r), decreasing = TRUE)[seq_len(id_n)]

  # set y limits
  if(is.null(arglist$ylim)) {
    arglist$ylim <- c(0, max(r, na.rm = TRUE))
  }

  if (xaxis == "fitted") {
    xv <- stats::fitted(model)
    rplot_raw(x = xv, y = r,
              labels = row.names(model$model),
              idd = idd,
              xlab = "fitted values",
              add_reference = FALSE,
              smooth = smooth,
              add_smooth = add_smooth,
              arglist = arglist,
              text_arglist = text_arglist,
              abline_arglist = list(),
              smooth_arglist = smooth_arglist,
              lines_arglist = lines_arglist,
              extendrange_f = 0)
  } else {
    # determine first-order variables in original model
    all_preds <- all.vars(stats::formula(model))[-1]
    # get all terms from predictors formula
    xv <- stats::model.frame(predictors,
                             model$model[,-1, drop = FALSE])
    # only keep the intersection of all_preds and names(preds_x)
    # it should filter out interaction, offset, and I terms.
    xv <- xv[intersect(all_preds, names(xv))]
    # save current par values
    curpar <- graphics::par(no.readonly = TRUE)
    # adjust for multiple plots
    graphics::par(mfrow = auto_mfrow(ncol(xv)))
    # get predictor names
    xnames = names(xv)
    # residual plot for each predictor
    for (j in seq_along(xnames)) {
      is_factor <- is.factor(xv[,j])
      temp_idd <- idd
      if (is_factor) {
        temp_idd <- integer(0)
      }
      rplot_raw(x = xv[,j], y = r,
                idd = temp_idd,
                labels = row.names(model$model),
                xlab = xnames[j],
                add_reference = ifelse(!is_factor,
                                       FALSE,
                                       FALSE),
                smooth = smooth,
                add_smooth = ifelse(!is_factor,
                                    add_smooth,
                                    FALSE),
                arglist = arglist,
                text_arglist = text_arglist,
                abline_arglist = list(),
                smooth_arglist = smooth_arglist,
                lines_arglist = lines_arglist,
                extendrange_f = 0)
    }
    # proper exit
    on.exit(graphics::par(curpar))
  }
}
