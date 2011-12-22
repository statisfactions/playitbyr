context("expscale")

test_that("scales to desired output", {
  test <- c(1, 1000, 1e+06, 1e+09, 1e+12, 1e+15)
  desired <- c(6, 7, 8, 9, 10, 11)
  out <- exp.scale(test, min = 6, max = 11)
  expect_equal(desired, out)
})
