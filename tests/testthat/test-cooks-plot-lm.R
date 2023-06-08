if (require("vdiffr")) {
  test_that("cooks_plot produces correct results", {
    lmod <- lm(price ~ sqft_living, data = home_sales)
    p <- function() cooks_plot(lmod)
    expect_doppelganger("default cooks_plot", p)
    p <- function() cooks_plot(lmod, id_n = 6)
    expect_doppelganger("cooks_plot custom 1", p)
    p <- function() {
      cooks_plot(lmod, add_reference = FALSE)
    }
    expect_doppelganger("cooks_plot no reference", p)
    p <- function() {
      cooks_plot(lmod, id_n = 1,
                  text_arglist = list(col = "blue", cex = 2),
                  abline_arglist = list(col = "red", lwd = 2))
    }
    expect_doppelganger("cooks_plot custom 2", p)
  })
}