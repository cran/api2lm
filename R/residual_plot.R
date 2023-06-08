#' Plot residuals of a fitted model
#'
#' \code{residual_plot} plots the residuals of a fitted model.
#'
#' @param model A fitted model.
#' @param ... Currently unimplemented.
#' @author Joshua French
#' @export
#' @examples
#' lmod <- lm(Girth ~ Height, data = trees)
#' residual_plot(lmod)
#' @export
residual_plot <- function(model, ...) {
  UseMethod("residual_plot")
}