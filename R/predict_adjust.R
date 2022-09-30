#' Adjust prediction intervals for multiple comparisons
#'
#' A function to produce adjusted confidence/prediction
#' intervals for predicted mean/new responses with a
#' family-wise confidence level of at least \code{level} for
#' \code{lm} objects (not applicable if no adjustment is
#' used). Internally, the function is a slight revision of
#' the code used in the \code{\link[stats]{predict.lm}}
#' function.
#'
#' Let \code{a = 1 - level}. All intervals are computed
#' using the formula \code{prediction +/- m * epesd}, where
#' \code{m} is a multiplier and \code{epesd} is the
#' estimated standard deviation of the prediction error of
#' the \code{estimate}.
#'
#' \code{method = "none"} (no correction) produces the
#' standard t-based confidence intervals with multiplier
#' \code{stats::qt(1 - a/2, df = object$df.residual)}.
#'
#' \code{method = "bonferroni"} produces Bonferroni-adjusted
#' intervals that use the multiplier \code{m = stats::qt(1 -
#' a/(2 * k), df = object$df.residual)}, where \code{k} is
#' the number of intervals being produced.
#'
#' The Working-Hotelling and Scheffe adjustments are distinct;
#' the Working-Hotelling typically is related to a multiple comparisons adjustment
#' for confidence intervals of the response mean while the Scheffe adjustment is typically
#' related to a multiple comparisons adjustment for prediction intervals
#' for a new response. However, references often uses these names
#' interchangeably, so we use them equivalently in this function.
#'
#' \code{method = "wh"} (Working-Hotelling) or
#' \code{method = "scheffe"} and \code{interval =
#' "confidence"} produces Working-Hotelling-adjusted intervals that
#' use the multiplier \code{m = sqrt(p * stats::qf(level,
#' df1 = p, df2 = object$df.residual))}, where \code{p} is
#' the number of estimated coefficients in the model.
#'
#' \code{method = "wh"} (Working-Hotelling) or
#' \code{method = "scheffe"} and \code{interval =
#' "prediction"} produces Scheffe-adjusted intervals that
#' use the multiplier \code{m = sqrt(k * stats::qf(level,
#' df1 = k, df2 = object$df.residual))}, where \code{k} is
#' the number of intervals being produced.
#'
#' @inheritParams stats::predict.lm
#' @param method A character string indicating the type of
#'   adjustment to make. The default choice is
#'   \code{"none"}. The other available options are
#'   \code{"bonferroni"}, \code{"wh"} (Working-Hotelling),
#'   and \code{"scheffe"}.
#'
#' @return \code{predict_adjust} produces:
#'
#' A vector of predictions if \code{interval = "none"}.
#'
#' A matrix of predictions and bounds with
#' column names \code{fit}, \code{lwr}, and \code{upr} if
#' \code{interval} is set. For \code{type = "terms"} this is
#' a matrix with a column per term and may have an attribute
#' \code{"constant"}.
#'
#' If \code{se.fit} is \code{TRUE}, a
#' list with the following components is returned:
#' \itemize{
#'  \item{\code{fit}}{: vector or matrix as above}
#'  \item{\code{se.fit}}{: standard error of predicted means}
#'  \item{\code{residual.scale}}{: residual standard deviations}
#'  \item{\code{df}}{: degrees of freedom for residual}
#' }
#' @export
#' @seealso \code{\link[stats]{predict.lm}}
#' @references
#' Bonferroni, C. (1936). Teoria statistica delle classi e
#' calcolo delle probabilita. Pubblicazioni del R Istituto
#' Superiore di Scienze Economiche e Commericiali di
#' Firenze, 8, 3-62.
#'
#' Working, H., & Hotelling, H. (1929). Applications of the
#' theory of error to the interpretation of trends. Journal
#' of the American Statistical Association, 24(165A), 73-85.
#' doi:10.1080/01621459.1929.10506274
#'
#' Kutner, M. H., Nachtsheim, C. J., Neter, J., & Li, W.
#' (2004). Applied Linear Statistical Models, 5th edition.
#' New York: McGraw-Hill/Irwin.
#' @examples
#' fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
#' newdata <- as.data.frame(rbind(
#'                apply(mtcars, 2, mean),
#'                apply(mtcars, 2, median)))
#' predict_adjust(fit, newdata = newdata,
#'                interval = "confidence",
#'                method = "none")
#' predict_adjust(fit, newdata = newdata,
#'                interval = "confidence",
#'                method = "bonferroni")
#' predict_adjust(fit, newdata = newdata,
#'                interval = "confidence",
#'                method = "wh")
#' predict_adjust(fit, newdata = newdata,
#'                interval = "prediction",
#'                method = "scheffe")
predict_adjust <- function (object, newdata, se.fit = FALSE, scale = NULL,
          df = Inf,
          interval = c("none", "confidence", "prediction"), level = 0.95,
          type = c("response", "terms"), method = "none",
          terms = NULL, na.action = stats::na.pass,
          pred.var = res.var/weights, weights = 1, ...)  {
  method <- match.arg(method, c("none", "bonferroni", "wh", "scheffe"))
  tt <- terms(object)
  if (!inherits(object, "lm"))
    warning("calling predict.lm(<fake-lm-object>) ...")
  if (missing(newdata) || is.null(newdata)) {
    mm <- X <- stats::model.matrix(object)
    mmDone <- TRUE
    offset <- object$offset
  } else {
    Terms <- stats::delete.response(tt)
    m <- stats::model.frame(Terms, newdata, na.action = na.action,
                     xlev = object$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses")))
      stats::.checkMFClasses(cl, m)
    X <- stats::model.matrix(Terms, m, contrasts.arg = object$contrasts)
    offset <- rep(0, nrow(X))
    if (!is.null(off.num <- attr(tt, "offset")))
      for (i in off.num) offset <- offset + eval(attr(tt,
                                                      "variables")[[i + 1]], newdata)
    if (!is.null(object$call$offset))
      offset <- offset + eval(object$call$offset, newdata)
    mmDone <- FALSE
  }
  n <- length(object$residuals)
  p <- object$rank
  p1 <- seq_len(p)
  piv <- if (p)
    qr_lm(object)$pivot[p1]
  if (p < ncol(X) && !(missing(newdata) || is.null(newdata)))
    warning("prediction from a rank-deficient fit may be misleading")
  beta <- object$coefficients
  predictor <- drop(X[, piv, drop = FALSE] %*% beta[piv])
  if (!is.null(offset))
    predictor <- predictor + offset
  interval <- match.arg(interval)
  if (interval == "prediction") {
    if (missing(newdata))
      warning("predictions on current data refer to _future_ responses\n")
    if (missing(newdata) && missing(weights)) {
      w <- stats::weights(object)
      if (!is.null(w)) {
        weights <- w
        warning("assuming prediction variance inversely proportional to weights used for fitting\n")
      }
    }
    if (!missing(newdata) && missing(weights) && !is.null(object$weights) &&
        missing(pred.var))
      warning("Assuming constant prediction variance even though model fit is weighted\n")
    if (inherits(weights, "formula")) {
      if (length(weights) != 2L)
        stop("'weights' as formula should be one-sided")
      d <- if (missing(newdata) || is.null(newdata))
        stats::model.frame(object)
      else newdata
      weights <- eval(weights[[2L]], d, environment(weights))
    }
  }
  type <- match.arg(type)
  if (se.fit || interval != "none") {
    w <- object$weights
    res.var <- if (is.null(scale)) {
      r <- object$residuals
      rss <- sum(if (is.null(w)) r^2 else r^2 * w)
      df <- object$df.residual
      rss/df
    }
    else scale^2
    if (type != "terms") {
      if (p > 0) {
        XRinv <- if (missing(newdata) && is.null(w))
          qr.Q(qr_lm(object))[, p1, drop = FALSE]
        else X[, piv] %*% qr.solve(qr.R(qr_lm(object))[p1,
                                                       p1])
        ip <- drop(XRinv^2 %*% rep(res.var, p))
      }
      else ip <- rep(0, n)
    }
  }
  if (type == "terms") {
    if (!mmDone) {
      mm <- stats::model.matrix(object)
      mmDone <- TRUE
    }
    aa <- attr(mm, "assign")
    ll <- attr(tt, "term.labels")
    hasintercept <- attr(tt, "intercept") > 0L
    if (hasintercept)
      ll <- c("(Intercept)", ll)
    aaa <- factor(aa, labels = ll)
    asgn <- split(order(aa), aaa)
    if (hasintercept) {
      asgn$"(Intercept)" <- NULL
      avx <- colMeans(mm)
      termsconst <- sum(avx[piv] * beta[piv])
    }
    nterms <- length(asgn)
    if (nterms > 0) {
      predictor <- matrix(ncol = nterms, nrow = NROW(X))
      dimnames(predictor) <- list(rownames(X), names(asgn))
      if (se.fit || interval != "none") {
        ip <- matrix(ncol = nterms, nrow = NROW(X))
        dimnames(ip) <- list(rownames(X), names(asgn))
        Rinv <- qr.solve(qr.R(qr_lm(object))[p1, p1])
      }
      if (hasintercept)
        X <- sweep(X, 2L, avx, check.margin = FALSE)
      unpiv <- rep.int(0L, NCOL(X))
      unpiv[piv] <- p1
      for (i in seq.int(1L, nterms, length.out = nterms)) {
        iipiv <- asgn[[i]]
        ii <- unpiv[iipiv]
        iipiv[ii == 0L] <- 0L
        predictor[, i] <- if (any(iipiv > 0L))
          X[, iipiv, drop = FALSE] %*% beta[iipiv]
        else 0
        if (se.fit || interval != "none")
          ip[, i] <- if (any(iipiv > 0L))
            as.matrix(X[, iipiv, drop = FALSE] %*% Rinv[ii,
                                                        , drop = FALSE])^2 %*% rep.int(res.var,
                                                                                       p)
        else 0
      }
      if (!is.null(terms)) {
        predictor <- predictor[, terms, drop = FALSE]
        if (se.fit)
          ip <- ip[, terms, drop = FALSE]
      }
    }
    else {
      predictor <- ip <- matrix(0, n, 0L)
    }
    attr(predictor, "constant") <- if (hasintercept)
      termsconst
    else 0
  }
  # number of predictions
  k <- length(predictor)
  # adjusted level
  if (method == "none") {
    adj_level <- max(1 - k * (1 - level), 0)
  } else {
    adj_level <- level
  }
  if (interval != "none") {
    if (method == "none") {
      tfrac <- stats::qt((1 - level)/2, df)
    } else if (method == "bonferroni") {
      tfrac <- stats::qt((1 - level)/2/k, df)
    } else if (any(is.element(method, c("wh", "scheffe"))) &
                   interval == "prediction") {
      tfrac <- -sqrt(k * stats::qf(level, df1 = k, df2 = df))
    } else if (any(is.element(method, c("wh", "scheffe"))) &
               interval == "confidence") {
      tfrac <- -sqrt(p * stats::qf(level, df1 = p, df2 = df))
    }

    hwid <- tfrac * switch(interval, confidence = sqrt(ip),
                           prediction = sqrt(ip + pred.var))
    if (type != "terms") {
      predictor <- cbind(predictor, predictor + hwid %o%
                           c(1, -1))
      colnames(predictor) <- c("fit", "lwr", "upr")
    }
    else {
      if (!is.null(terms))
        hwid <- hwid[, terms, drop = FALSE]
      lwr <- predictor + hwid
      upr <- predictor - hwid
    }
  }
  if (se.fit || interval != "none") {
    se <- sqrt(ip)
    if (type == "terms" && !is.null(terms) && !se.fit)
      se <- se[, terms, drop = FALSE]
  }
  if (missing(newdata) && !is.null(na.act <- object$na.action)) {
    predictor <- stats::napredict(na.act, predictor)
    if (se.fit)
      se <- stats::napredict(na.act, se)
  }
  if (type == "terms" && interval != "none") {
    if (missing(newdata) && !is.null(na.act)) {
      lwr <- stats::napredict(na.act, lwr)
      upr <- stats::napredict(na.act, upr)
    }
    list(fit = predictor, se.fit = se, lwr = lwr, upr = upr,
         df = df, residual.scale = sqrt(res.var))
  }
  else if (se.fit)
    structure(list(fit = predictor, se.fit = se, df = df, residual.scale = sqrt(res.var)),
              class = c("predict_adjust", "list"),
              method = method, adj_level = adj_level,
              interval = interval)
  else structure(predictor,
                 class = c("predict_adjust", class(predictor)),
                 method = method, adj_level = adj_level,
                 interval = interval)
}

qr_lm <- function (x, ...) {
  if (is.null(x$qr)) {
    stop("lm object does not have a proper 'qr' component.\n Rank zero or should not have used lm(.., qr=FALSE).")
  } else {
    x$qr
  }
}