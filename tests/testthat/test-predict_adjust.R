# p. 62 of ALSM5
test_that("predict_adjust produces correct confidence limits", {
  data(toluca)
  lmod <- lm(work_hours ~ lot_size, data = toluca)
  newx <- data.frame(lot_size = 100)
  # unadjusted
  pcu <- predict_adjust(lmod,
                        newdata = newx,
                        level = 0.90,
                        interval = "confidence",
                        method = "none")
  pcui <- matrix(c(round(pcu[,2:3], 1)), nrow = 2)
  pcu_truth <- predict(lmod, newdata = newx, level = 0.90,
                       interval = "confidence")
  pcui_truth <- matrix(c(round(pcu_truth[,2:3], 1)), nrow = 2)

  # bonferroni adjustment
  # ppb <- predict_adjust(lmod,
  #                       newdata = newx,
  #                       level = 0.90,
  #                       interval = "prediction",
  #                       method = "bonferroni")
  # ppbi <- matrix(c(round(ppb[,2:3], 1)), nrow = 2)
  # ppbi_truth <- cbind(c(167.3, 149.1), c(214.9, 199.2))

  # scheffe adjustment
  pcs <- predict_adjust(lmod,
                        newdata = newx,
                        level = 0.90,
                        interval = "confidence",
                        method = "scheffe")
  pcsi <- matrix(c(round(pcs[,2:3], 1)), nrow = 2)
  pcsi_truth <- matrix(c(387.2, 451.6))
  expect_equal(pcui, pcui_truth)
  # expect_equal(ppbi, ppbi_truth)
  expect_equal(pcsi, pcsi_truth)
})

# p. 159 of ALSM5
test_that("predict_adjust produces correct confidence limits", {
  data(toluca)
  lmod <- lm(work_hours ~ lot_size, data = toluca)
  newx <- data.frame(lot_size = c(30, 65, 100))
  # unadjusted
  pcu <- predict_adjust(lmod,
                        newdata = newx,
                        level = 0.90,
                        interval = "confidence",
                        method = "none")
  pcui <- matrix(c(round(pcu[,2:3], 1)), nrow = 3)
  pcu_truth <- predict(lmod, newdata = newx, level = 0.90,
                       interval = "confidence")
  pcui_truth <- matrix(c(round(pcu_truth[,2:3], 1)), nrow = 3)

  # bonferroni adjustment
  pcb <- predict_adjust(lmod,
                        newdata = newx,
                        level = 0.90,
                        interval = "confidence",
                        method = "bonferroni")
  pcbi <- matrix(c(round(pcb[,2:3], 1)), nrow = 3)
  pcbi_truth <- cbind(c(131.1, 272, 387.1),
                      c(207.9, 316.9, 451.7))

  # wh adjustment
  pcs <- predict_adjust(lmod,
                        newdata = newx,
                        level = 0.90,
                        interval = "confidence",
                        method = "scheffe")
  pcsi <- matrix(c(round(pcs[,2:3], 1)), nrow = 3)
  pcsi_truth <- cbind(c(131.2, 272, 387.2),
                      c(207.8, 316.8, 451.6))
  expect_equal(pcui, pcui_truth)
  expect_equal(pcbi, pcbi_truth)
  expect_equal(pcsi, pcsi_truth)
})

# p. 160 of ALSM5
test_that("predict_adjust produces correct prediction limits", {
  data(toluca)
  lmod <- lm(work_hours ~ lot_size, data = toluca)
  newx <- data.frame(lot_size = c(80, 100))
  # unadjusted
  psu <- predict_adjust(lmod,
                        newdata = newx,
                        level = 0.95,
                        interval = "confidence",
                        method = "none")
  psui <- matrix(c(round(psu[,2:3], 1)), nrow = 2)
  psu_truth <- predict(lmod, newdata = newx, level = 0.95,
                       interval = "confidence")
  psui_truth <- matrix(c(round(psu_truth[,2:3], 1)), nrow = 2)

  # bonferroni adjustment
  psb <- predict_adjust(lmod,
                        newdata = newx,
                        level = 0.95,
                        interval = "prediction",
                        method = "bonferroni")
  psbi <- matrix(c(round(psb[,2:3], 1)), nrow = 2)
  psbi_truth <- cbind(c(228.3, 297.4),
                      c(467.7, 541.4))

  # scheffe adjustment
  pss <- predict_adjust(lmod,
                        newdata = newx,
                        level = 0.95,
                        interval = "prediction",
                        method = "scheffe")
  pssi <- matrix(c(round(pss[,2:3], 1)), nrow = 2)
  pssi_truth <- round(cbind(c(348 - 2.616 * 49.91, 419.4 - 2.616 * 50.87),
                      c(348 + 2.616 * 49.91, 419.4 + 2.616 * 50.87)), 1)
  expect_equal(psui, psui_truth)
  expect_equal(psbi, psbi_truth)
  expect_equal(pssi, pssi_truth)
})

# p. 247 of ALSM5
test_that("predict_adjust produces correct prediction limits", {
  data(dwaine)
  lmod <- lm(sales ~ targetpop + dpi, data = dwaine)
  newx <- data.frame(targetpop = c(65.4, 53.1),
                     dpi = c(17.6, 17.7))
  # unadjusted
  ppu <- predict_adjust(lmod,
                        newdata = newx,
                        level = 0.90,
                        interval = "prediction",
                        method = "none")
  ppui <- matrix(c(round(ppu[,2:3], 1)), nrow = 2)
  ppu_truth <- predict(lmod, newdata = newx, level = 0.90,
                       interval = "prediction")
  ppu_truth <- predict(lmod, newdata = newx, level = 0.90,
                       interval = "prediction")
  ppui_truth <- matrix(c(round(ppu_truth[,2:3], 1)), nrow = 2)

  # bonferroni adjustment
  ppb <- predict_adjust(lmod,
                        newdata = newx,
                        level = 0.90,
                        interval = "prediction",
                        method = "bonferroni")
  ppbi <- matrix(c(round(ppb[,2:3], 1)), nrow = 2)
  ppbi_truth <- cbind(c(167.3, 149.1), c(214.9, 199.2))

  # scheffe adjustment
  pps <- predict_adjust(lmod,
                        newdata = newx,
                        level = 0.90,
                        interval = "prediction",
                        method = "scheffe")
  ppsi <- matrix(c(round(pps[,2:3], 1)), nrow = 2)
  yhat <- c(191.10, 174.15)
  spred <- c(11.35, 11.93)
  ppsi_truth <- round(cbind(yhat - 2.29 * spred,
                            yhat + 2.29 * spred),
                      1)
  expect_equal(ppui, ppui_truth)
  expect_equal(ppbi, ppbi_truth)
  expect_equal(ppsi, ppsi_truth)
})



