#' Index plot helper function
#'
#' @param x x-values to plot
#' @param y y-values to plot
#' @param idd Identified observations
#' @param add_reference Logical value
#' @param arglist Named list for plot
#' @param text_arglist Named list for text
#' @param abline_arglist Named list for abline
#' @param labels The labels to use for the identified points.
#' @inheritParams index_plot_lm
#' @return NULL
index_plot_raw <- function(x, y,
                           idd,
                           labels,
                           add_reference,
                           arglist,
                           text_arglist,
                           abline_arglist,
                           extendrange_f) {
  # set appropriate y-axis limits
  if (is.null(arglist$ylim)) {
    if (length(idd) >= 1) {
      arglist$ylim <- grDevices::extendrange(y, f = extendrange_f)
    } else {
      arglist$ylim <- range(y)
    }
  }
  if (is.null(arglist$type)) {
    arglist$type <- "h"
  }

  # set appropriate x-axis labels and limits
  if (is.null(arglist$xlab)) {
    arglist$xlab <- "index"
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
}

#' Check arguments of residual_plot.lm
#'
#' @inheritParams index_plot_lm
#' @keywords internal
#' @return NULL
arg_check_index_plot_lm <-
  function(model, stat, id_n, add_reference,
           text_arglist, abline_arglist,
           extendrange_f) {
    if (!is.element("lm", class(model))) {
      stop("model must be an object with class 'lm'")
    }
    if (!is.function(stat)) {
      stop("stat must be a function")
    }
    if (length(id_n) != 1 | min(id_n) < 0) {
      stop("id_n must be a positive value")
    }
    if (length(add_reference) != 1 | !is.logical(add_reference)) {
      stop("add_reference must be a single logical value")
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


#' Create plot elements for index_plot_lm
#'
#' @inheritParams index_plot_lm
#' @return A list with elements \code{x}, \code{y},
#' and \code{z}.
#' @keywords internal
index_plot_lm_elements <- function(model, stat) {
  returned_stats = do.call(stat, list(model))
  if (is.null(model$na.action)) {
    y <- returned_stats
    x <- seq_along(y)
    labs <- names(returned_stats)
  } else {
    if (is.element("omit", class(model$na.action))) {
      which_na <- unname(c(model$na.action))
      # total number of observations
      n <- stats::nobs(model) + length(which_na)
      y <- rep(NA, n)
      labs <- rep(NA, n)
      y[-which_na] <- returned_stats
      x <- seq_along(y)
      labs[-which_na] <- names(returned_stats)
    } else if (is.element("exclude",
                          class(model$na.action))) {
      y <- returned_stats
      x <- seq_along(y)
      labs <- names(returned_stats)
    } else {
      stop("That na.action is not supported.")
    }
  }
  return(list(x = x, y = y, labs = labs))
}

#' Get elements for dfbetas index plot
#'
#' @inheritParams dfbetas_plot
#' @param returned_stats A vector of dfbetas statistics
#' @return A list with elements \code{x}, \code{y},
#' and \code{z}.
#' @keywords internal
index_plot_dfbetas_elements <-
  function(model, returned_stats) {
  if (is.null(model$na.action)) {
    y <- returned_stats
    x <- seq_along(y)
    labs <- names(returned_stats)
  } else {
    if (is.element("omit", class(model$na.action))) {
      which_na <- unname(c(model$na.action))
      # total number of observations
      n <- stats::nobs(model) + length(which_na)
      y <- rep(NA, n)
      labs <- rep(NA, n)
      y[-which_na] <- returned_stats
      x <- seq_along(y)
      labs[-which_na] <- names(returned_stats)
    } else if (is.element("exclude",
                          class(model$na.action))) {
      y <- returned_stats
      x <- seq_along(y)
      labs <- names(returned_stats)
    } else {
      stop("That na.action is not supported.")
    }
  }
  return(list(x = x, y = y, labs = labs))
}