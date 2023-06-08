#' dfbeta index plots
#'
#' \code{dfbeta_plot} creates an index plot of the
#' \code{dfbeta} statistics for each regressor.
#'
#' A horizontal reference line is added at +/- the
#' estimated standard error of each coefficient by
#' default if \code{add_reference} is \code{TRUE}.
#'
#' @inheritParams dfbetas_plot
#' @author Joshua French
#' @seealso \code{\link[graphics]{plot}},
#'   \code{\link[graphics]{text}},
#'   \code{\link[graphics]{abline}}
#'   \code{\link[stats]{dfbeta}}.
#' @export
#' @examples
#' lmod <- lm(murder ~ hs_grad + urban + poverty + single,
#'            data = crime2009)
#' dfbeta_plot(lmod)
#' dfbeta_plot(lmod, regressors = ~ hs_grad + poverty,
#'             id_n = 1)
dfbeta_plot <-
  function(model,
           id_n = 3,
           regressors = ~ .,
           add_reference = TRUE,
           ...,
           text_arglist = list(),
           abline_arglist = list(),
           extendrange_f = 0.08) {
  arglist <- list(...)

  # determine first-order variables in original model
  dfbeta_stats <- stats::dfbeta(model)
  dfbeta_names <- colnames(dfbeta_stats)

  regressor_names <-
    labels(stats::terms(regressors,
                        data = stats::model.matrix(model)))
  # Correct inconsistency in (Intercept) name
  if (any(is.element("`(Intercept)`", regressor_names))) {
    wi <- which(regressor_names == "`(Intercept)`")
    regressor_names[wi] <- "(Intercept)"
  }
  rcheck <- is.element(regressor_names, dfbeta_names)
  if(any(!rcheck)) {
    rbad <- paste(regressor_names[!rcheck], collapse = ", ")
    stop(paste("The regressors provided do not match",
               "the regressor names produced by dfbeta.",
               "The problem appears to be", rbad))

  }
  # get overlapping dfbeta variables
  dfbeta_names <- intersect(dfbeta_names, regressor_names)
  dfbeta_stats <-
    dfbeta_stats[, dfbeta_names, drop = FALSE]

  # determine ses of estimates for plotting
  dfbeta_ses <- sqrt(diag(stats::vcov(model)))
  w_ses <- match(dfbeta_names, names(dfbeta_ses))
  dfbeta_ses <- dfbeta_ses[w_ses]

  # save current par values
  curpar <- graphics::par(no.readonly = TRUE)
  # adjust for multiple plots
  graphics::par(mfrow = auto_mfrow(length(dfbeta_names)))
  if (is.null(arglist$xlab)) {
    arglist$xlab <- "index"
  }

  set_h <- ifelse(is.null(abline_arglist$h), TRUE, FALSE)

  # index plot for each dfbeta statistic
  for (j in seq_along(dfbeta_names)) {
    # get elements for current dfbeta statistic
    temp_elements <-
      index_plot_dfbetas_elements(
        model, dfbeta_stats[,j]
        )
    temp_x <- temp_elements$x
    temp_y <- temp_elements$y
    temp_labels <- temp_elements$lab
    temp_idd <- order(abs(temp_y),
                      decreasing = TRUE)[seq_len(id_n)]

    # set y-axis label
    arglist$ylab <- dfbeta_names[j]

    if (add_reference & set_h) {
      abline_arglist$h <- c(-1, 1) * dfbeta_ses[j]
    }

    index_plot_raw(x = temp_x,
                   y = temp_y,
                   idd = temp_idd,
                   labels = temp_labels,
                   add_reference = add_reference,
                   arglist = arglist,
                   text_arglist = text_arglist,
                   abline_arglist = abline_arglist,
                   extendrange_f = extendrange_f)
  }
  # proper exit
  on.exit(graphics::par(curpar))
}

#' Check arguments of dfbeta_plot
#'
#' @inheritParams residual_plot.lm
#' @keywords internal
#' @return NULL
arg_check_dfbeta_plot <-
  function(model, id_n, regressors, add_reference,
           text_arglist, abline_arglist,
           extendrange_f) {
    if (!is.element("lm", class(model))) {
      stop("model must be an lm object")
    }
    if (length(id_n) != 1 | min(id_n) < 0) {
      stop("id_n must be a positive value")
    }
    if (!is.element("formula", class(regressors))) {
      stop("regressors must be a formula")
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