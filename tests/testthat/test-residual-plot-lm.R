if (require("vdiffr")) {
test_that("residual_plot.lm produces correct results", {
  lmod <- lm(Petal.Length ~ Sepal.Length + Species,
             data = iris)
  p <- function() residual_plot(lmod)
  expect_doppelganger("default residual_plot", p)
  p <- function() residual_plot(lmod, rtype = "standardized")
  expect_doppelganger("standardized residual_plot", p)
  p <- function() residual_plot(lmod, rtype = "studentized")
  expect_doppelganger("studentized residual_plot", p)
  p <- function() residual_plot(lmod, rtype = "loo")
  expect_doppelganger("loo residual_plot", p)
  p <- function() residual_plot(lmod, xaxis = "pred", id_n = 2)
  expect_doppelganger("residual_plot all pres", p)
  p <- function() {
    residual_plot(lmod, xaxis = "pred",
                  predictors = ~ Sepal.Length, id_n = 2)
  }
  expect_doppelganger("residual_plot Sepal.Length", p)
  p <- function() {
    residual_plot(lmod, xaxis = "pred",
                  predictors = ~ Species, id_n = 2)
  }
  expect_doppelganger("residual_plot Species", p)
  p <- function() {
    residual_plot(lmod,
                  text_arglist = list(col = "blue", cex = 2),
                  abline_arglist = list(lwd = 2, lty = 2,
                                        col = "brown"),
                  lines_arglist = list(col = "purple"),
                  )
  }
  expect_doppelganger("residual_plot custom", p)
})
}