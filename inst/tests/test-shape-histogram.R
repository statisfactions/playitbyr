context("histogram")

test_that("basic histogram example works", {
  x <- sonify(iris, sonaes(pitch = Sepal.Length)) + sonfacet(Species) +
    shape_histogram(length = 3, tempo = 1800)
  soncompare(x, "test-shape-histogram.wav")
})

