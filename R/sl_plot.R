#' Spread-level plot of a fitted model
#'
#' \code{sl_plot} creates a spread-level plot
#'  for a fitted model.
#'
#' @param model A fitted model.
#' @param ... Currently unimplemented.
#' @author Joshua French
#' @export
#' @examples
#' lmod <- lm(Girth ~ Height, data = trees)
#' sl_plot(lmod)
#' @export
sl_plot <- function(model, ...) {
  UseMethod("sl_plot")
}