set.seed(719)

require(audio)

context("scalings")

test_that("scaling function sanity checks", {
  expect_equal(rep(5, 5), linear_scale(rep(22, 5), NULL, c(0, 10)))
  expect_equal(1:5, linear_scale(16:20*300, NULL, c(1,5)))
  expect_equal(c(NA, 1, 5, NA, NA), linear_scale(16:20, 17:18, c(1, 5)))
  expect_equal(rep(5, 5), exp_scale(rep(22, 5), NULL, c(0, 10)))
  expect_equal(1:3, exp_scale(c(1, 10, 100), NULL, c(1, 3)))
  expect_equal(c(NA, 2, 3, NA), exp_scale(c(1, 10, 100, -5), c(10, 100) , c(2, 3)))
  expect_equal(c(2, 2), linear_scale(c(1, 1), c(0, 1), c(1, 2)))
  expect_equal(c(2, 2), exp_scale(c(1, 1), c(0.1, 1), c(1, 2)))
  expect_error(exp_scale(c(1, 1), c(0, 1), c(1, 2)))
})

test_that("rest of package can handle NA output from linear_scale or in source dataframe", {
})



