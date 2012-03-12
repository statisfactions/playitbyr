set.seed(719)

require(tuneR)

context("dotplot")

test_that("dotplot matches", {
  x <- sonify(iris[1:10,], sonaes(time = Petal.Length)) + shape_dotplot() + scale_time_continuous(c(0, 5))
  out <- tempfile()
  sonsave(x, out)
  curr <- readWave(out)
  comp <- readWave(system.file("testdata/test-dotplot.wav", package = "playitbyr"))
  expect_equal(curr, comp)
})
