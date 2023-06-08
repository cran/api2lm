if (require("vdiffr")) {
test_that("sl_plot.lm produces correct results", {
  lmod <- lm(Petal.Length ~ Sepal.Length + Species,
             data = iris)
  p <- function() sl_plot(lmod)
  expect_doppelganger("default sl_plot", p)
  p <- function() sl_plot(lmod, rtype = "studentized")
  expect_doppelganger("studentized sl_plot", p)
  p <- function() sl_plot(lmod, xaxis = "pred", id_n = 2)
  expect_doppelganger("sl_plot all pres", p)
  p <- function() {
    sl_plot(lmod, xaxis = "pred",
                  predictors = ~ Sepal.Length, id_n = 2)
  }
  expect_doppelganger("sl_plot Sepal_Length", p)
  p <- function() {
    sl_plot(lmod, xaxis = "pred",
            predictors = ~ Species, id_n = 2)
  }
  expect_doppelganger("sl_plot Species", p)
  p <- function() {
    sl_plot(lmod,
            text_arglist = list(col = "blue", cex = 2),
            lines_arglist = list(col = "purple"),
            )
  }
  expect_doppelganger("sl_plot custom", p)
})
}