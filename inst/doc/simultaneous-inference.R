## -----------------------------------------------------------------------------
fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)

## -----------------------------------------------------------------------------
confint(fit)

## -----------------------------------------------------------------------------
library(api2lm)
confint_adjust(fit)

## -----------------------------------------------------------------------------
(ci_b <- confint_adjust(fit, method = "bonferroni"))

## -----------------------------------------------------------------------------
confint_adjust(fit, method = "wh")

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
plot(ci_b)

## -----------------------------------------------------------------------------
plot(ci_b, parm = c("hp", "disp"))

## -----------------------------------------------------------------------------
plot(ci_b, parm = c("hp", "disp"), mar = c(4.1, 4.1, 2.1, 2.1))

## -----------------------------------------------------------------------------
library(ggplot2)
autoplot(ci_b, parm = c("hp", "disp"))

## -----------------------------------------------------------------------------
# observations for which to predict the mean response
newdata <- as.data.frame(rbind(
               apply(mtcars, 2, mean),
               apply(mtcars, 2, median)))
# unadjusted intervals
predict_adjust(fit, newdata = newdata,
               interval = "confidence",
               method = "none")
# bonferroni-adjusted intervals
predict_adjust(fit, newdata = newdata,
               interval = "confidence",
               method = "bonferroni")
# working-hotelling-adjusted intervals
predict_adjust(fit, newdata = newdata,
               interval = "confidence",
               method = "wh")

## -----------------------------------------------------------------------------
# observations for which to predict the mean response
newdata <- as.data.frame(rbind(
               apply(mtcars, 2, mean),
               apply(mtcars, 2, median),
               apply(mtcars, 2, quantile, prob = 0.25),
               apply(mtcars, 2, quantile, prob = 0.75)))
# unadjusted intervals
predict_adjust(fit, newdata = newdata,
               interval = "prediction",
               method = "none")
# bonferroni-adjusted intervals
predict_adjust(fit, newdata = newdata,
               interval = "prediction",
               method = "bonferroni")
# scheffe-adjusted intervals
predict_adjust(fit, newdata = newdata,
               interval = "prediction",
               method = "scheffe")

