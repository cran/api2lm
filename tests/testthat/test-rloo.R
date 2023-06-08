test_that("rloo, rjackknife, rdeleted produce correct results", {
  data(dwaine)
  lmod <- lm(sales ~ targetpop + dpi, data = dwaine)
  infl <- lm.influence(lmod, do.coef = FALSE)
  l <- rloo(lmod, infl = infl)
  stud_loo <- l * sqrt(1 - infl$hat)/infl$sigma
  # compare studentized residuals
  expect_equal(rstudent(lmod), l * sqrt(1 - infl$hat)/infl$sigma)
  # compare rloo to rjackknife, rdeleted
  expect_equal(l, rjackknife(lmod))
  expect_equal(l, rdeleted(lmod))
})