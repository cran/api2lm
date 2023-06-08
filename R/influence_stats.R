#' Influence statistics
#'
#' \code{influence_stats} returns a data frame with
#' influence-related statistics ordered from the largest
#' to smallest magnitude of the \code{criterion}.
#' @inheritParams influence_plot
#' @inheritParams utils::head
#'
#' @return A data frame of influence-related statistics.
#' @author Joshua French
#' @seealso
#'   \code{\link[stats]{rstandard}},
#'   \code{\link[stats]{rstudent}},
#'   \code{\link[stats]{hatvalues}}
#'   \code{\link[stats]{cooks.distance}},
#'   \code{\link[stats]{dffits}}
#' @export
#' @examples
#' lmod <- lm(murder ~ hs_grad + urban + poverty + single,
#'            data = crime2009)
#' influence_stats(lmod, n = 3)
#' influence_stats(lmod, rtype = "stan", crit = "df")
influence_stats <-
  function(model, n = 6L,
           rtype = c("studentized", "standardized"),
           criterion = c("cooks", "dffits")) {
  if (!is.element("lm", class(model))) {
    stop("model must be an lm object")
  }
  # get arguments
  rtype <- match.arg(rtype, c("studentized","standardized"))
  criterion <- match.arg(criterion, c("cooks", "dffits"))

  # get stats
  x <- stats::hatvalues(model)
  y <- get_residuals(x = model, rtype = rtype)

  if (criterion == "cooks") {
    crit_stat <- stats::cooks.distance(model)
  } else {
    crit_stat <- stats::dffits(model)
  }

  # create data frame
  output_df <- data.frame(crit_stat, x, y)
  names(output_df) <- c(criterion, "leverage", rtype)

  # order crit_stat
  o <- order(abs(crit_stat), decreasing = TRUE)

  # return ordered data frame
  utils::head(output_df[o,], n = n)
}