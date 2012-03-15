set.seed(719)

require(tuneR)

context("dotplot")

test_that("dotplot matches", {
  x <- sonify(iris[1:10,], sonaes(time = Petal.Length)) + shape_dotplot(jitter = 0.3)
  out <- tempfile()
  sonsave(x, out)
  curr <- readWave(out)
  comp <- readWave(system.file("testdata/test-dotplot.wav", package = "playitbyr"))
  expect_equal(curr, comp)
})
