set.seed(719)

require(tuneR)

context("non-numeric")

test_that("data mappings can be non-numeric", {
  x <- sonify(iris[41:50,], sonaes(time = Sepal.Length, pitch = Species)) + shape_scatter()
  outfile <- paste(tempfile(), ".wav", sep="")
  sonsave(x, outfile)
  curr <- readWave(outfile)
  unlink(outfile)
  comp <- readWave(system.file("testdata/test-nonnumericworks.wav", package = "playitbyr"))
  expect_equal(curr, comp)
})
