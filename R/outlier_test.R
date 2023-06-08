#' Identify outliers
#'
#' \code{outlier_test} returns the observations identified
#' as an outlier based on the Bonferroni correction for a
#' studentized residuals.
#' @inheritParams residual_plot.lm
#' @param n The number of outliers to return. The default is
#'   all outliers.
#' @param alpha The Bonferroni-adjusted threshold at which
#'   an outlier is identified. The default is \code{0.05}.
#'
#' @return A data frame with the outliers.
#' @seealso \code{\link[stats]{rstudent}},
#'   \code{\link[stats]{p.adjust}}
#' @export
#' @examples
#' lmod <- lm(price ~ sqft_living, data = home_sales)
#' outlier_test(lmod)
#' outlier_test(lmod, alpha = 1, n = 7)
#' lmod2 <- lm(Petal.Length ~ Sepal.Length + Species, iris)
#' outlier_test(lmod2)
outlier_test <-
  function(model,
           n = stats::nobs(model),
           alpha = 0.05) {
  arg_check_outlier_test(model, n, alpha)
  r <- stats::rstudent(model)
  r <- r[order(abs(r), decreasing = TRUE)]
  p <- stats::pt(abs(r), df = stats::df.residual(model) - 1,
                 lower.tail = FALSE)
  padj <- stats::p.adjust(p, method = "bonferroni")
  output_df <- data.frame(stat = r, pvalue = p,
                          adj_pvalue = padj)
  output_df <- output_df[output_df$adj_pvalue <= alpha, ]
  if(nrow(output_df) == 0) {
    cat(paste("No outliers detected at the",
              "Bonferroni-adjusted", alpha,
              "threshold.\n"))
    return(invisible(output_df))
  } else {
    if (nrow(output_df) > n) {
      return(utils::head(output_df, n = n))
    } else {
      return(output_df)
    }
  }
}

#' Check arguments of \code{outlier_test}
#'
#' @inheritParams outlier_test
#' @keywords internal
arg_check_outlier_test <- function(model, n, alpha) {
  if (!is.element("lm", class(model))) {
    stop("model must be an lm object")
  }
  if (length(n) != 1 | !is.numeric(n)) {
    stop("n must be a single number")
  }
  if (n < 1) {
    stop("n must be 1 or more")
  }
  if (length(alpha) != 1 | !is.numeric(alpha)) {
    stop("alpha must be a single number")
  }
  if (alpha <= 0 | alpha > 1) {
    stop("alpha must be in (0, 1]")
  }
}