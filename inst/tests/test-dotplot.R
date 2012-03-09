set.seed(719)

require(audio)

context("dotplot")

test_that("dotplot matches", {
  x <- sonify(iris[1:10,], sonaes(time = Petal.Length)) + shape_dotplot()
  out <- tempfile()
  sonsave(x, out)
  curr <- load.wave(out)
  load(system.file("testdata/dotplot.rda", package = "playitbyr"))
  expect_equal(curr, comp)
})
