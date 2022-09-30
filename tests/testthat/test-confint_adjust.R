# p. 245 of ALSM5
test_that("confint_adjust produces correct confidence limits", {
  data(dwaine)
  lmod <- lm(sales ~ targetpop + dpi, data = dwaine)

  # unadjusted
  ciu <- confint_adjust(lmod, parm = 2:3)
  ciui <- matrix(unlist(ciu[,3:4]), nrow = 2, ncol = 2)
  ciu_truth <- confint(lmod, parm = 2:3)
  ciui_truth <- matrix(c(ciu_truth), nrow = 2)

  # bonferroni adjustment
  cib <- confint_adjust(lmod, parm = 2:3, method = "bonferroni",
                        level = 0.90)
  cibi <- round(matrix(unlist(cib[,3:4]), nrow = 2, ncol = 2), 2)
  cibi_truth <- cbind(c(1.01, 0.83), c(1.90, 17.90))

  # wh adjustment
  cis <- confint_adjust(lmod, method = "wh", level = 0.90)
  cisi <- matrix(unlist(cis[,3:4]), nrow = 3, ncol = 2)

  newx = diag(3)
  ses <- sqrt(diag(vcov(lmod)))
  w <- sqrt(3 * stats::qf(0.90, 3, stats::df.residual(lmod)))
  coeffs <- unname(coef(lmod))
  cisi_truth  <- matrix(c(coeffs - w * ses, coeffs + w * ses), ncol = 2)

  expect_equal(ciui, ciui_truth)
  expect_equal(cibi, cibi_truth)

  # p. 158, modified
  # wh adjustment
  data(toluca)
  lmod <- lm(work_hours ~ lot_size, data = toluca)
  newx <- data.frame(lot_size = c(80, 100))
  cis <- confint_adjust(lmod, method = "wh", level = 0.90)
  cisi <- matrix(unlist(cis[,3:4]), nrow = 2, ncol = 2)

  coeffs <- unname(coef(lmod))
  ses <- sqrt(diag(vcov(lmod)))
  p <- length(coeffs)
  newx <- diag(p)
  w <- sqrt(p * stats::qf(0.90, p, stats::df.residual(lmod)))

  cisi_truth  <- matrix(c(coeffs - w * ses,
                          coeffs + w * ses),
                        ncol = 2)
  expect_equal(cisi, cisi_truth)
})
