if (require("vdiffr")) {
  test_that("dfbeta_plot produces correct results", {
    lmod <- lm(murder ~ hs_grad + urban + poverty + single,
               data = crime2009)
    p <- function() dfbeta_plot(lmod)
    expect_doppelganger("default dfbeta_plot", p)
    p <- function() dfbeta_plot(lmod, id_n = 6)
    expect_doppelganger("dfbeta_plot custom 1", p)
    p <- function() {
      dfbeta_plot(lmod, regressors = ~ hs_grad + poverty,
                  id_n = 1)
    }
    expect_doppelganger("dfbeta_plot 1 variable", p)
    p <- function() {
      dfbeta_plot(lmod, id_n = 1,
                   text_arglist = list(col = "blue", cex = 2),
                   abline_arglist = list(col = "red", lwd = 2, h = c(-0.2, 0.2)))
    }
    expect_doppelganger("dfbeta_plot custom 2", p)
  })
}