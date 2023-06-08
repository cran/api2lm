if (require("vdiffr")) {
test_that("leverage_plot produces correct results", {
  lmod <- lm(price ~ sqft_living, data = home_sales)
  p <- function() leverage_plot(lmod)
  expect_doppelganger("default leverage_plot", p)
  p <- function() leverage_plot(lmod, id_n = 6, ttype = "2mean")
  expect_doppelganger("leverage_plot 2mean", p)
  p <- function() {
    leverage_plot(lmod, id_n = 2, ttype = "custom",
                  threshold = 0.15)
  }
  expect_doppelganger("leverage_plot custom 1", p)
  p <- function() {
    leverage_plot(lmod, id_n = 3, ttype = "2mean",
                 abline_arglist = list(lty = 3, col = "blue"),
                 text_arglist = list(col = "brown", cex = 1.2))
  }
  expect_doppelganger("leverage_plot custom 2", p)
})
}