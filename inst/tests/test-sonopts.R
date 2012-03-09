set.seed(719)

require(audio)

require(testthat)


context("sonopts()")

test_that("bad parameters give errors", {
  expect_error(sonopts("yummy"), "rendering must be 'csound' or 'audio'")
  expect_error(sonopts(rendering = "csound", i = 1, blah = "blarg",
                       bloop = "blarf"),
               "Unrecognized csound parameters 'blah', 'bloop'")
  expect_error(sonopts(rendering = "audio", samp.rate = 321, i = 20),
               "Unrecognized audio parameters 'i'")
})

testlist <- structure(list(rendering = "csound",
                 orcfile = "whatwhat.orc",
                   f = list(c(1, 4, 3, 1, 1))), class = "sonopts")
sonoptsout <- sonopts("csound",
                      orcfile = "whatwhat.orc",
                      f = list(c(1, 4, 3, 1, 1)))

test_that("sonopt list outputs as expected", {
  expect_equal("sonopts", class(sonoptsout))
  expect_equal(testlist, sonoptsout)
})
                     
test_that("sonopt works with `+.sonify`", {
  testsonify <- structure(list(data = NULL, dataname = "NULL",
                               mapping = structure(list(), class = "sonaes"), 
                               scales = structure(list(), class = "sonscaling"),
                               sonlayers = NULL, opts = testlist),
                          class = "sonify")
  expect_equal(testsonify, sonify() + sonoptsout)
})

