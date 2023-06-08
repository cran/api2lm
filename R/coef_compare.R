#' Compare coefficients of 2 models
#'
#' Compare the coefficients to two fitted models. The models
#' must have the same coefficients.
#'
#' @param model1 A fitted model object from the
#'   \code{\link[stats]{lm}} function.
#' @param model2 A fitted model object from the
#'   \code{\link[stats]{lm}} function.
#' @param digits A positive integer indicating how many
#'   significant digits are to be used for numeric and
#'   complex numbers. This argument is passed to
#'   \code{\link[base]{format}}.
#' @param verbose A logical value indicating whether the
#'   matrix should be printed. The default is \code{TRUE}.
#'
#' @return A matrix.
#' @export
#' @examples
#' # fit model
#' lmod1 <- lm(murder ~ hs_grad + urban + poverty + single,
#'            data = crime2009)
#' #fit without DC
#' lmod2 <- lm(murder ~ hs_grad + urban + poverty + single,
#'             data = crime2009[-9, ])
#' #compare coefficients of models
#' coef_compare(lmod1, lmod2)
coef_compare <- function(model1, model2, digits = 3,
                         verbose = TRUE) {
  if (!is.element("lm", class(model1))) {
    stop("model 1 must be an lm object")
  }
  if (!is.element("lm", class(model1))) {
    stop("model 2 must be an lm object")
  }
  if (!identical(names(stats::coef(model1)),
                 names(stats::coef(model2)))) {
    stop("The models must have the same coefficients")
  }
  if (length(verbose) != 1 | !is.logical(verbose)) {
    stop("verbose must be a logical value")
  }
  temp = lapply(seq_len(length(stats::coef(model1))),
                combine_coefs,
                a = t(stats::summary.lm(model1)$coef[,1:2]),
                b = t(stats::summary.lm(model2)$coef[,1:2]),
                digits = digits)
  # combine lists into single matrix
  temp = do.call(rbind, temp)
  # print matrix
  if (verbose) {
    print(temp, quote = FALSE, right = TRUE)
  }
  return(invisible(temp))
}

#' Combine coefficients
#'
#' Combine coefficients and standard errors from two
#' fitted models
#'
#' @param i Index of coefficients
#' @param a Coefficients and standard errors from Model 1
#' @param b Coefficients and standard errors from Model 2
#' @inheritParams coef_compare
#' @return A character matrix
#' @keywords internal
combine_coefs <- function(i, a, b, digits = NULL) {
  # create initial matrix
  temp = cbind(c(a[,i], NA),
               c(b[,i], NA),
               c(100 * (b[,i] - a[,i])/a[,i], NA))
  row.names(temp)[1] <- colnames(a)[i]
  # format matrix
  temp <- format(temp, digits = digits)
  # replace NA with "  "
  temp[3, 1] <- sub("NA", "  ", temp[3, 1])
  temp[3, 2] <- sub("NA", "  ", temp[3, 1])
  temp[3, 3] <- sub("NA", "  ", temp[3, 1])
  # create column names
  colnames(temp) <- c("Model 1", "Model 2", "pct_change")
  # return temp
  return(temp)
}

# combine_coefs3 <- function(i, a, b, digits = NULL) {
#   # create initial matrix
#   temp = cbind(c(a[,i], NA),
#                c(b[,i], NA),
#                c(100 * (b[,i] - a[,i])/a[,i], NA))
#   # create column names
#   colnames(temp) <- c("Model 1", "Model 2", "pct_change")
#   # return temp
#   as.table(temp)
# }
#
# coef_compare3 <- function(model1, model2, digits = 3) {
#   if (!is.element("lm", class(model1))) {
#     stop("model 1 must be an lm object")
#   }
#   if (!is.element("lm", class(model1))) {
#     stop("model 2 must be an lm object")
#   }
#   if (!identical(names(stats::coef(model1)),
#                  names(stats::coef(model2)))) {
#     stop("The models must have the same coefficients")
#   }
#   temp = lapply(seq_len(length(stats::coef(model1))),
#                 combine_coefs3,
#                 a = t(stats::summary.lm(model1)$coef[,1:2]),
#                 b = t(stats::summary.lm(model2)$coef[,1:2]),
#                 digits = digits)
#   # combine lists into single table
#   temp = do.call(rbind, temp)
#   # print table
#   print.table(temp, right = TRUE)
#   return(invisible(temp))
# }
