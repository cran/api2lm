if (require("vdiffr")) {
test_that("influence_plot produces correct results", {
  lmod <- lm(murder ~ hs_grad + urban + poverty + single,
             data = crime2009)
  p <- function() influence_plot(lmod)
  expect_doppelganger("default influence_plot", p)
  p <- function() influence_plot(lmod, rtype = "stan", id_n = 4)
  expect_doppelganger("influence_plot stan", p)
})
}