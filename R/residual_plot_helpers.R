#' Set default values of abline_arglist
#'
#' Set the default values of abline_arglist if not
#' specified by the user.
#' @param abline_arglist A named list.
#' @return A named list.
#' @keywords internal
set_abline_arglist <- function(abline_arglist) {
  if (!is.list(abline_arglist)) {
    stop("abline_arglist must be a named list.")
  }
  if (is.null(abline_arglist$lty)) {
    abline_arglist$lty <- 3
  }
  if (is.null(abline_arglist$col)) {
    abline_arglist$col <- "grey"
  }
  if (is.null(abline_arglist$h)) {
    abline_arglist$h <- 0
  }
  return(abline_arglist)
}

#' Set default values of lines_arglist
#'
#' Set the default values of lines_arglist if not
#' specified by the user.
#' @param lines_arglist A named list.
#' @return A named list.
#' @keywords internal
set_lines_arglist <- function(lines_arglist) {
  if (!is.list(lines_arglist)) {
    stop("lines_arglist must be a named list.")
  }
  if (is.null(lines_arglist$col)) {
    lines_arglist$col <- 2
  }
  return(lines_arglist)
}


#' Set the default values of text_arglist if not
#' specified by the user.
#' @param text_arglist A named list.
#' @param x The x-values of the labels.
#' @param y The y-values of the labels.
#' @param labels The labels to plot.
#' @return A named list.
#' @keywords internal
set_text_arglist <- function(text_arglist, x, y, labels, idd) {
  if (!is.list(text_arglist)) {
    stop("text_arglist must be a named list.")
  }
  x_id <- x[idd]
  text_arglist$x <- x_id
  y_id <- y[idd]
  y_id[y_id < 0] <- y_id[y_id < 0] - graphics::strheight(" ")/3
  text_arglist$y <- y_id
  text_arglist$labels <- labels[idd]
  label_pos <- c(4, 2)
  usr <- graphics::par("usr")
  text_arglist$pos <- label_pos[(x_id > mean(usr[1:2])) + 1L]
  text_arglist$xpd <- TRUE
  text_arglist$offset <- 0.25
  if (is.null(text_arglist$cex)) {
    text_arglist$cex <- 0.75
  }
  return(text_arglist)
}

#' Automatically determine `mfrow`
#'
#' Automatically determine a reasonable choice for
#' for `mfrow` in the \code{\link[graphics]{par}}
#' function based on the number of plots `n`.
#'
#' @param n The number of plots.
#' @return A `numeric` vector of length 2.
#' @export
#' @keywords internal
#' auto_mfrow(4)
auto_mfrow <- function(n) {
  if (length(n) != 1)
    stop("n should be a single positive integer")
  if (length(n) < 1)
    stop("n should be a single positive integer")
  n <- round(n)
  if (n == 1) {
    return(c(1, 1))
  }
  else if (n == 2 | n == 3) {
    return(c(1, n))
  }
  else if (n == 4) {
    return(c(2, 2))
  }
  else if (n == 5 | n == 6) {
    return(c(2, 3))
  }
  else if (n > 6 & n <= 9) {
    return(c(3, 3))
  }
  else if (n > 9 & n <= 12) {
    return(c(3, 4))
  }
  else if (n > 12 & n <= 16) {
    return(c(4, 4))
  }
  else if (n > 16 & n <= 20) {
    return(c(4, 5))
  }
  else if (n > 21 & n <= 25) {
    return(c(5, 5))
  }
  else if (n > 26 & n <= 30) {
    return(c(5, 6))
  }
  else if (n > 31 & n <= 36) {
    return(c(6, 6))
  }
  else {
    stop("auto_mfrow only works for n <= 36.  user must choose size.")
  }
}

#' Helper function for residuals_plot.lm
#'
#' @param x x-values to plot
#' @param y y-values to plot
#' @param idd Identified observations
#' @param add_reference Logical value
#' @param add_smooth Logical value
#' @param arglist Named list for plot
#' @param text_arglist Named list for text
#' @param abline_arglist Named list for abline
#' @param smooth_arglist Named list for smooth
#' @param lines_arglist Named list for lines of smooth
#' @param xlab The x-axis label
#' @param smooth The function for smoothing. Needs a `formula` argument.
#' @param labels The labels to use for the identified points.
#' @inheritParams residual_plot.lm
#' @return NULL
rplot_raw <- function(x, y,
                      idd,
                      labels,
                      xlab,
                      smooth,
                      add_reference,
                      add_smooth,
                      arglist,
                      text_arglist,
                      abline_arglist,
                      smooth_arglist,
                      lines_arglist,
                      extendrange_f) {

  # set appropriate y-axis limits
  if (is.null(arglist$ylim)) {
    if (length(idd) >= 1) {
      arglist$ylim <- grDevices::extendrange(y, f = extendrange_f)
    } else {
      arglist$ylim <- range(y)
    }
  }

  # set appropriate x-axis labels and limits
  if (is.null(arglist$xlab)) {
    arglist$xlab <- xlab
  }
  if (is.null(arglist$xlim) & !is.factor(x)) {
    arglist$xlim <- range(x)
  }

  # main plot
  arglist$x <- x
  arglist$y <- y
  do.call(plot, arglist)

  if (add_reference) {
    abline_arglist <- set_abline_arglist(abline_arglist)
    do.call(graphics::abline, abline_arglist)
  }
  if (length(idd) >= 1) {
    text_arglist <- set_text_arglist(text_arglist,
                                     x = x,
                                     y = y,
                                     labels = labels,
                                     idd = idd)
    do.call(graphics::text, text_arglist)
  }
  if (add_smooth) {
    if (is.null(smooth_arglist$span)) {
      smooth_arglist$span <- 2/3
    }
    if (is.null(smooth_arglist$degree)) {
      smooth_arglist$degree <- 1
    }
    if (is.null(smooth_arglist$family)) {
      smooth_arglist$family <- "symmetric"
    }
    if (is.null(smooth_arglist$surface)) {
      smooth_arglist$surface <- "direct"
    }
    smooth_arglist$formula <- y ~ x
    ox = order(x)
    smooth_arglist$data <- data.frame(x = x[ox], y = y[ox])
    smooth_mod <- do.call(smooth, smooth_arglist)
    lines_arglist <- set_lines_arglist(lines_arglist)
    lines_arglist$x <- x[ox]
    lines_arglist$y <- stats::fitted(smooth_mod)
    do.call(graphics::lines, lines_arglist)
  }
}

#' Check arguments of residual_plot.lm
#'
#' @inheritParams residual_plot.lm
#' @keywords internal
#' @return NULL
arg_check_residual_plot_lm <-
  function(rtype, xaxis, id_n, smooth, add_reference,
           add_smooth, text_arglist, abline_arglist,
           smooth_arglist, lines_arglist, extendrange_f) {
  if (length(rtype) != 1) {
    stop("rtype must have length 1")
  }
  if (length(xaxis) != 1) {
    stop("xaxis must have length 1")
  }
  if (length(id_n) != 1 | min(id_n) < 0) {
    stop("id_n must be a positive value")
  }
  if (!is.function(smooth)) {
    stop("smooth must be a function")
  }
  if (!is.element("formula", names(formals(smooth)))) {
    stop("smooth must have a formula argument")
  }
  if (length(add_reference) != 1 | !is.logical(add_reference)) {
    stop("add_reference must be a single logical value")
  }
  if (length(add_smooth) != 1 | !is.logical(add_smooth)) {
    stop("add_smoth must be a single logical value")
  }
  if (!is.list(text_arglist)) {
    stop("text_arglist must be a named list")
  }
  if (!is.list(abline_arglist)) {
    stop("abline_arglist must be a named list")
  }
  if (!is.list(smooth_arglist)) {
    stop("smooth_arglist must be a named list")
  }
  if (!is.list(lines_arglist)) {
    stop("lines_arglist must be a named list")
  }
  if (!is.numeric(extendrange_f)) {
    stop("extendrange_f must be numeric")
  }
  if (length(extendrange_f) < 1 | length(extendrange_f) > 2) {
    stop("extendrange_f must be a vector of length 1 or 2")
  }
}
