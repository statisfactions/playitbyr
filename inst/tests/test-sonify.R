require(testthat)
require(playitbyr)

context("sonify()")

test_that("default rendering is csound", {
  expect_equal(sonify()$opts$rendering, "csound")
})


