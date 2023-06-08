#' Influence plots
#'
#' \code{influence_plot} creates an influence plot for a
#' fitted \code{lm} object. The y-axis is either the
#' studentized (the default) or standardized residuals
#' versus the leverage values for each observation. The size
#' of the point associated with each observation is
#' proportional to the value of the Cook's distance (the
#' default) or the DFFITS statistic for the observation.
#' \cr\cr Details about the different types of residuals are
#' discussed in the \code{\link[api2lm]{get_residuals}}
#' function.
#'
#' The range of the \code{criterion} statistic is mapped to
#' \code{cex_pt = size[2]^2 - size[1]^2} and then the size
#' of the points is \code{sqrt(cex_pt)}.
#'
#' If \code{add_reference} is \code{TRUE}, then horizontal
#' reference lines are added at the \eqn{\alpha/2} and
#' \eqn{1-\alpha/2} quantiles of a t distribution with
#' degrees of freedom given by
#' \code{stats::df.residual(model)}.
#'
#' If \code{add_reference} is \code{TRUE}, then vertical
#' reference lines are added at \eqn{2p/n} and \eqn{0.5}
#' where \eqn{p=}\code{length(stats::coef(model))} and
#' \eqn{n=}\code{stats::nobs(model)}.
#'
#' The vertical position of the reference lines can be
#' customized by setting the \code{h} argument of
#' \code{abline_arglist}. The horizontal position of the
#' reference lines can be customized by setting the \code{v}
#' argument of \code{abline_arglist}.
#'
#' @inheritParams residual_plot.lm
#' @param rtype The residual type to plot on the y-axis. The
#'   default is \code{"studentized"}. The other option is
#'   \code{"standardized"}.
#' @param criterion The criterion that decides the size of
#'   the points. The default is \code{"cooks"}. The other
#'   option is \code{"dffits"}.
#' @param id_n The number of points to identify with labels
#'   with respect to largest absolute criterion. The default
#'   is \code{3} labels.
#' @param size A numeric vector of length 2 that provides
#' guidelines for the size of the points.
#' @param alpha The default quantile used for the horizontal
#'   reference lines. The default is 0.05. See Details.
#' @author Joshua French
#' @seealso \code{\link[graphics]{plot}},
#'   \code{\link[graphics]{text}},
#'   \code{\link[graphics]{abline}},
#'   \code{\link[stats]{rstandard}},
#'   \code{\link[stats]{rstudent}},
#'   \code{\link[stats]{hatvalues}}
#'   \code{\link[stats]{cooks.distance}},
#'   \code{\link[stats]{dffits}}
#' @export
#' @examples
#' lmod <- lm(murder ~ hs_grad + urban + poverty + single,
#'            data = crime2009)
#' # studentized residuals vs leverage
#' influence_plot(lmod, id_n = 3)
#' # standardized residuals vs leverage
#' influence_plot(lmod, rtype = "stan")
#' # similar plot from plot.lm
#' plot(lmod, which = 5)
influence_plot <-
  function(model,
           rtype = c("studentized", "standardized"),
           criterion = c("cooks", "dffits"),
           id_n = 3,
           add_reference = TRUE,
           alpha = 0.05,
           size = c(1, 4.8),
           ...,
           text_arglist = list(),
           abline_arglist = list(),
           extendrange_f = 0.08) {
  arglist <- list(...)
  # get arguments
  rtype <- match.arg(rtype, c("studentized","standardized"))
  criterion <- match.arg(criterion, c("cooks", "dffits"))
  # argument checking
  arg_check_influence_plot(model, id_n, add_reference,
                           alpha, size, text_arglist,
                           abline_arglist, extendrange_f)
  if(is.null(arglist$ylab)) {
    arglist$ylab <- paste(rtype, "residuals")
  }
  if(is.null(arglist$xlab)) {
    arglist$xlab <- "leverage"
  }

  x <- stats::hatvalues(model)
  y <- get_residuals(x = model, rtype = rtype)
  if (criterion == "cooks") {
    crit_stat <- stats::cooks.distance(model)
  } else {
    crit_stat <- stats::dffits(model)
  }

  size <- size^2
  size_min <- min(size, na.rm = TRUE)
  crit_min <- min(crit_stat, na.rm = TRUE)
  rout <-  max(size, na.rm = TRUE) - size_min
  rin <- max(crit_stat, na.rm = TRUE) - crit_min
  cex_pt <- size_min + rout/rin * (crit_stat - crit_min)
  arglist$cex <- sqrt(cex_pt)

  # id points
  idd <- order(crit_stat, decreasing = TRUE)[seq_len(id_n)]

  # save current par values
  curpar <- graphics::par(no.readonly = TRUE)
  # add reference lines
  if (add_reference) {
    if (is.null(abline_arglist$h)) {
      dfr <- stats::df.residual(model)
      abline_arglist$h <-
        stats::qt(c(alpha/2, 1-alpha/2), df = dfr)
    }
    if (is.null(abline_arglist$v)) {
      n <- stats::nobs(model)
      p <- length(stats::coef(model))
      abline_arglist$v <- c(2 * p / n, 0.5)
    }
  }

  rplot_raw(x = x, y = y,
            labels = names(y),
            idd = idd,
            xlab = arglist$xlab,
            add_reference = add_reference,
            smooth = stats::loess,
            add_smooth = FALSE,
            arglist = arglist,
            text_arglist = text_arglist,
            abline_arglist = abline_arglist,
            smooth_arglist = list(),
            lines_arglist = list(),
            extendrange_f = extendrange_f)

  # proper exit
  on.exit(graphics::par(curpar))
}

#' Check arguments of dfbetas_plot
#'
#' @inheritParams influence_plot
#' @keywords internal
#' @return NULL
arg_check_influence_plot <-
  function(model, id_n, add_reference, alpha, size,
           text_arglist, abline_arglist,
           extendrange_f) {
    if (!is.element("lm", class(model))) {
      stop("model must be an lm object")
    }
    if (length(id_n) != 1 | min(id_n) < 0) {
      stop("id_n must be a positive value")
    }
    if (length(add_reference) != 1 | !is.logical(add_reference)) {
      stop("add_reference must be a single logical value")
    }
    if (length(alpha) != 1 | !is.numeric(alpha)) {
      stop("alpha must be a single numeric value")
    }
    if (alpha <= 0 | alpha >= 1) {
      stop("alpha must be in the interval (0, 1)")
    }
    if (length(size) != 2 | !is.numeric(alpha)) {
      stop("size must be a numeric vector of length 2")
    }
    if (!(size[1] <= size[2])) {
      stop("size[1] must be less than size[2]")
    }
    if (!is.list(text_arglist)) {
      stop("text_arglist must be a named list")
    }
    if (!is.list(abline_arglist)) {
      stop("abline_arglist must be a named list")
    }
    if (!is.numeric(extendrange_f)) {
      stop("extendrange_f must be numeric")
    }
    if (length(extendrange_f) < 1 | length(extendrange_f) > 2) {
      stop("extendrange_f must be a vector of length 1 or 2")
    }
  }