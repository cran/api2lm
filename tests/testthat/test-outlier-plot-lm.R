if (require("vdiffr")) {
test_that("outlier_plot produces correct results", {
  lmod <- lm(price ~ sqft_living, data = home_sales)
  p <- function() outlier_plot(lmod)
  expect_doppelganger("default outlier_plot", p)
  p <- function() outlier_plot(lmod, id_n = 1)
  expect_doppelganger("outlier_plot 2", p)
  p <- function() {
    outlier_plot(lmod, id_n = 2, add_reference = FALSE)
  }
  expect_doppelganger("outlier_plot custom 1", p)
  p <- function() {
    outlier_plot(lmod, id_n = 2, alpha = 0.4,
                 abline_arglist = list(lty = 3, col = "blue"),
                 text_arglist = list(col = "brown", cex = 1.2))
  }
  expect_doppelganger("outlier_plot custom 2", p)
})
}