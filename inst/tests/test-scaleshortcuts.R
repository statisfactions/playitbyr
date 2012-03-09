require(audio)

context("scaleShortcuts")

test_that("scaling shortcuts produce as expected with no dmin, dmax", {
  ## linear
  reference_continuous <- sonscaling(time = list(3, 10, linear_scale))
  test_continuous <- scale_time_continuous(3, 10)
  expect_equal(reference_continuous, test_continuous)
  ## exponential
  reference_exp <- sonscaling(time = list(3, 10, exp_scale))
  test_exp <- scale_time_exp(3, 10)
  expect_equal(reference_exp, test_exp)
})

test_that("scaling shortcuts as expected with dmin, dmax", {
  ## linear
  reference_continuous <- structure(list(time = structure(list(min = 3, max = 10,
                                                    scaling.function = function (column, 
                                                                                min, max) 
                                       {
                                         dmin <- -10
                                         dmax <- 20
                                         linear_scale(c(dmin, dmax, column), min, max)[-(1:2)]
                                       }), .Names = c("min", "max", "scaling.function"))), .Names = "time", class = "sonscaling")
  test_continuous <- scale_time_continuous(3, 10, -10, 20)
  expect_equal(reference_continuous, test_continuous)
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

test_that("scalings work when all the same value", {
  expect_equal(rep(5, 5), linear_scale(rep(22, 5), 0, 10))
  expect_equal(rep(2, 5), exp_scale(rep(2321, 5), 1, 3))
})
          
