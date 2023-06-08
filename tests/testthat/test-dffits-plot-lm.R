if (require("vdiffr")) {
  test_that("dffits_plot produces correct results", {
    lmod <- lm(price ~ sqft_living, data = home_sales)
    p <- function() dffits_plot(lmod)
    expect_doppelganger("default dffits_plot", p)
    p <- function() leverage_plot(lmod, id_n = 6)
    expect_doppelganger("dffits_plot custom 1", p)
    p <- function() {
      leverage_plot(lmod, add_reference = FALSE)
    }
    expect_doppelganger("dffits_plot no reference", p)
    p <- function() {
      dffits_plot(lmod, id_n = 1,
                  text_arglist = list(col = "blue", cex = 2),
                  abline_arglist = list(col = "red", lwd = 2))
    }
    expect_doppelganger("dffits_plot custom 2", p)
  })
}