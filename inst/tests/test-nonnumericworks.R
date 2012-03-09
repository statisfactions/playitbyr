set.seed(719)

require(audio)

context("non-numeric")

test_that("data mappings can be non-numeric", {
  x <- sonify(iris, sonaes(time = Sepal.Length, pitch = Species)) + shape_scatter()
  outfile <- paste(tempfile(), ".wav", sep="")
  sonsave(x, outfile)
  curr <- load.wave(outfile)
  unlink(outfile)
  load(system.file("testdata/nonnumeric.rda", package = "playitbyr"))
  expect_equal(curr, comp)
})
