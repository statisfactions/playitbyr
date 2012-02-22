require(testthat)


context("sonify()")

test_that("default rendering is csound", {
  expect_equal(sonify()$opts$rendering, "csound")
})


