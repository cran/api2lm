#' Adjust confidence intervals for multiple comparisons
#'
#' A function to produce adjusted confidence intervals with a family-wise
#' confidence level of at least \code{level} for
#' \code{lm} objects (not applicable if no adjustment is used).
#' Internally, the function is a slight revision of the code
#' used in the \code{\link[stats]{confint.lm}} function.
#'
#' Let \code{a = 1 - level}. Let \code{p} be the number of
#' estimated coefficients in the fitted model. All intervals are computed
#' using the formula \code{estimate +/- m * ese}, where
#' \code{m} is a multiplier and \code{ese} is the estimated
#' standard error of the \code{estimate}.
#'
#' \code{method = "none"} (no correction) produces the
#' standard t-based confidence intervals with multiplier
#' \code{qt(1 - a/2, df = object$df.residual)}.
#'
#' \code{method = "bonferroni"} produces Bonferroni-adjusted
#' intervals that use the multiplier \code{m = qt(1 - a/(2 *
#' k), df = object$df.residual)}, where \code{k} is the
#' number of intervals being produced.
#'
#' \code{method = "wh"} produces Working-Hotelling-adjusted
#' intervals that are valid for all linear combinations of
#' the regression coefficients, which uses the multiplier
#' \code{m = sqrt(p * qf(level, df1 = p, df2 =
#' object$df.residual))}.
#'
#' @inheritParams stats::confint.lm
#' @param method A character string indicating the type of
#'   adjustment to make. The default choice is
#'   \code{"none"}. The other option is
#'   \code{"bonferroni"}.
#'
#' @return A \code{confint_adjust} object, which is simply a
#'   a \code{data.frame} with columns \code{term},
#'   \code{lwr} (the lower confidence limit), and \code{upr}
#'   (the upper confidence limit).
#' @seealso \code{\link[stats]{confint.lm}}
#' @export
#' @references
#' Bonferroni, C. (1936). Teoria statistica
#'   delle classi e calcolo delle probabilita. Pubblicazioni
#'   del R Istituto Superiore di Scienze Economiche e
#' Commericiali di Firenze, 8, 3-62.
#'
#' Working, H., & Hotelling, H. (1929). Applications of the
#' theory of error to the interpretation of trends. Journal
#' of the American Statistical Association, 24(165A), 73-85.
#' doi:10.1080/01621459.1929.10506274
#'
#' Kutner, M. H., Nachtsheim, C. J., Neter, J., & Li, W.
#' (2004). Applied Linear Statistical Models, 5th edition.
#' New York: McGraw-Hill/Irwin. (p. 230)
#' @examples
#' ## an extension of the documentation for confint.lm
#' fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
#' # standard intervals
#' confint_adjust(fit)
#' # bonferroni-adjusted intervals
#' (cib <- confint_adjust(fit, method = "b"))
#' # plot results
#' plot(cib)
#' plot(cib, parm = c("hp", "disp"))
#' if (require(ggplot2)) {
#'   autoplot(cib)
#'   autoplot(cib, parm = c("hp", "disp"))
#' }
#' #' working-hotelling-adjusted intervals
#' (ciwh <- confint_adjust(fit, method = "wh"))
confint_adjust <- function(object, parm, level = 0.95, method = "none") {
  # match method
  method <- match.arg(method, c("none", "bonferroni", "wh"))
  # estimated coefficients
  cf <- stats::coef(object)
  # estimated standard errors
  ses <- sqrt(diag(stats::vcov(object)))
  # select variables
  pnames <- names(ses)
  if (is.matrix(cf)) {
    cf <- stats::setNames(as.vector(cf), pnames)
  }
  if (missing(parm)) {
    parm <- pnames
  } else if (is.numeric(parm)) {
    parm <- pnames[parm]
  }
  # alpha/2, 1-alpha/2
  a <- (1 - level)/2
  # number of intervals
  k <- length(parm)
  p <- object$rank
  if (method == "none") {
    fac <- stats::qt(c(a, 1 - a), object$df.residual)
    adj_level = max(1 - k * (1 - level), 0)
  } else if (method == "bonferroni") {
    fac <- stats::qt(c(a/k, 1 - a/k), object$df.residual)
    adj_level = level
  } else if (method == "wh") {
    fac <- c(-1, 1) * sqrt(p * stats::qf(level, df1 = p, df2 = object$df.residual))
    adj_level = level
  }

  # format returned object
  pct <- format_perc_api2lm(c(a, 1 - a), 3)
  ci <- array(NA_real_,
              dim = c(length(parm), 2L),
              dimnames = list(parm, pct))
  ci[] <- cf[parm] + ses[parm] %o% fac
  ci <- data.frame(term = row.names(ci),
                   estimate = cf[parm],
                   lwr = ci[,1],
                   upr = ci[,2])
  attributes(ci)$method <- method
  attributes(ci)$adj_level <- adj_level
  class(ci) <- c("confint_adjust", class(ci))
  ci
}
