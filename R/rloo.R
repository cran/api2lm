#' Compute leave-one-out residuals
#'
#' \code{rloo} computes the leave-one-out residuals of
#' \code{model}. \cr\cr
#' \code{rjacknife} and \code{rdeleted} are
#' aliases for \code{rloo}.
#'
#' @param model a fitted model object from the \code{\link[stats]{lm}} function.
#' @param ... Currently unimplemented
#' @author Joshua French
#' @seealso \code{\link[api2lm]{rloo.lm}}
#' @export
#' @examples
#' lmod <- lm(Girth ~ Height, data = trees)
#' rloo(lmod)
#' @export
rloo <- function(model, ...) {
  UseMethod("rloo")
}

#' @rdname rloo
#' @export
rdeleted <- function(model, ...) {
  UseMethod("rdeleted")
}

#' @rdname rloo
#' @export
rjackknife <- function(model, ...) {
  UseMethod("rjackknife")
}