#' Compute leave-one-out residuals for `lm` objects.
#'
#' \code{rloo.lm} computes the leave-one-out residuals of
#' the \code{lm} object stored in \code{model}. \cr\cr
#' \code{rjackknife.lm} and \code{rdeleted.lm} are aliases
#' for \code{rloo.lm}.
#'
#' Let \eqn{\hat{\epsilon}_i} denote the residual of the
#' \eqn{i}th observation and \eqn{h_i} denote the leverage
#' value of the \eqn{i}th observation The leave-one-out residual for observation \eqn{i} is
#' computed as
#'
#' \deqn{l_i = \frac{\hat{\epsilon}_i}{1-h_i}.}
#'
#' @param model a fitted model object from the \code{\link[stats]{lm}} function.
#' @param infl influence structure as returned by \code{\link[stats]{lm.influence}}.
#' @param res (possibly weighted) residuals, with proper default.
#' @param ... Currently unimplemented
#' @author Joshua French
#' @examples
#' lmod <- lm(Girth ~ Height, data = trees)
#' rloo(lmod)
#' @export
rloo.lm <- function(model,
                    infl = stats::lm.influence(model, do.coef = FALSE),
                    res = infl$wt.res,
                    ...) {
  res <- res/(1 - infl$hat)
  res[is.infinite(res)] <- NaN
  res
}

#' @rdname rloo.lm
#' @export
rdeleted.lm <- rloo.lm

#' @rdname rloo.lm
#' @export
rjackknife.lm <- rloo.lm