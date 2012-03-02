context("scaleShortcuts")

test_that("scaling shortcuts produce as expected with no dmin, dmax", {
  ## linear
  reference_linear <- sonscaling(time = list(3, 10, linear_scale))
  test_linear <- scale_time_linear(3, 10)
  expect_equal(reference_linear, test_linear)
  ## exponential
  reference_exp <- sonscaling(time = list(3, 10, exp_scale))
  test_exp <- scale_time_exp(3, 10)
  expect_equal(reference_exp, test_exp)
})

test_that("scaling shortcuts as expected with dmin, dmax", {
  ## linear
  reference_linear <- structure(list(time = structure(list(min = 3, max = 10,
                                                    scaling.function = function (column, 
                                                                                min, max) 
                                       {
                                         dmin <- -10
                                         dmax <- 20
                                         linear_scale(c(dmin, dmax, column), min, max)[-(1:2)]
                                       }), .Names = c("min", "max", "scaling.function"))), .Names = "time", class = "sonscaling")
  test_linear <- scale_time_linear(3, 10, -10, 20)
  expect_equal(reference_linear, test_linear)
  ## exponential
    reference_exp <- structure(list(time = structure(list(min = 3, max = 10,
                                                    scaling.function = function (column, 
                                                                                min, max) 
                                       {
                                         dmin <- 5
                                         dmax <- 22
                                         exp_scale(c(dmin, dmax, column), min, max)[-(1:2)]
                                       }), .Names = c("min", "max", "scaling.function"))), .Names = "time", class = "sonscaling")
  test_exp <- scale_time_exp(3, 10, 5, 22)
  expect_equal(reference_exp, test_exp)
})

