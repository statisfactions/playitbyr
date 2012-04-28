context("sonopts()")

testlist <- structure(list(rendering = "csound",
                 orcfile = "whatwhat.orc",
                   f = list(c(1, 4, 3, 1, 1))), class = "sonopts")
sonoptsout <- sonopts(orcfile = "whatwhat.orc",
                      f = list(c(1, 4, 3, 1, 1)))

test_that("sonopt list outputs as expected", {
  expect_equal("sonopts", class(sonoptsout))
  expect_equal(testlist, sonoptsout)
})
                     
test_that("sonopt works with `+.sonify`", {
  testsonify <- structure(list(data = NULL,
                               mapping = structure(list(), class = "sonaes"), 
                               scales = structure(list(), class = "sonscaling"),
                               sonlayers = NULL, opts = testlist, sonfacet = NULL),
                          class = "sonify")
  expect_equal(testsonify, sonify() + sonoptsout)
})

