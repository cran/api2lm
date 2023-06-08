#' #' Scatter plot of `regsubsets` object
#' #'
#' #' `plot_regsubsets` creates a scatter plot of the desired model selection criterion
#' #' versus the number of non-intercept regressors on the model.
#' #'
#' #' @param x An object of class `regsubsets` produced by the `regsubsets` function from the leaps package.
#' #' @param statistic One of `aic`, `bic`, `rsq`, `adjr2`, `var`, or `cp`.
#' #' @param best_pch The `pch` symbol used to identify the model optimizing the selection criterion. The default is `19`.
#' #' @param ... Additional arguments passed to the `plot` function.
#' #'
#' #' @return A plot.
#' #' @export
#' #'
#' #' @examples
#' #'
#' plot_regsubsets <- function(x, statistic, best_pch = 19, ...) {
#'   if (!is.element("regsubsets", class(x))) {
#'     stop("x must be a regsubsets object")
#'   }
#'   if (x$nbest != 1) {
#'     stop("The plots only work with x$nbest == 1")
#'   }
#'   statistic <- pmatch(statistic, c("aic", "bic", "rsq", "adjr2", "var", "cp"))
#'   arglist <- list(...)
#'   rss <- x$bound[-1]
#'   nr <- seq_along(rss)
#'   p <- seq_along(rss) + 1
#'   nr
#'   n <- x$nn
#'   srs <- summary(x)
#'
#'   if (statistic == "aic") {
#'     ll <-  -(n * log(rss/n) + n * log(2 * pi) + n)/2
#'     wmin <- which.min(AIC)
#'     AIC <- - 2 * ll + 2 * (p + 1)
#'     plot(AIC ~ p, ...)
#'     points(p[wmin], AIC[wmin], pch = best_pch)
#'   } else if (statistic == "bic") {
#'     ll <-  -(n * log(rss/n) + n * log(2 * pi) + n)/2
#'     BIC <- - 2 * ll + log(n) * (p + 1)
#'     wmin <- which.min(BIC)
#'     plot(AIC ~ p, ...)
#'     points(p[wmin], BIC[wmin], pch = best_pch)
#'   } else if (statistic == "rsq") {
#'     wmax <- which.max(srs$rsq)
#'     if (is.null(arglist$ylab)) {
#'       arglist$ylab <- "R-squared"
#'     }
#'     do.call(plot, c(list(formula = srs$rsq ~ p), arglist))
#'     points(p[wmax], srs$rsq[wmax], pch = best_pch)
#'   } else if (statistic == "adjr2") {
#'     wmax <- which.max(srs$adjr2)
#'     if (is.null(arglist$ylab)) {
#'       arglist$ylab <- "Adjusted R-squared"
#'     }
#'     do.call(plot, c(list(formula = srs$adjr2 ~ p), arglist))
#'     points(p[wmax], srs$adjr2[wmax], pch = best_pch)
#'   } else if (statistic == "cp") {
#'     wmin <- which.min(abs(srs$cp - p))
#'     if (is.null(arglist$ylab)) {
#'       arglist$ylab <- expression("Mallow's C[p]")
#'     }
#'     do.call(plot, c(list(formula = srs$cp ~ p), arglist))
#'     abline(0, 1)
#'     points(p[wmin], srs$cp[wmin], pch = best_pch)
#'   } else if (statistic == "var") {
#'     wmin <- which.max(srs$adjr2)
#'     if (is.null(arglist$ylab)) {
#'       arglist$ylab <- "estimated variance"
#'     }
#'     sigmasq <- rss/(n-p)
#'     do.call(plot, c(list(formula = sigmasq ~ p), arglist))
#'     points(p[wmin], sigmasq[wmin], pch = best_pch)
#'   }
#' }
