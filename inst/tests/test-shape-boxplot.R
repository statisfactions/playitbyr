context("boxplot")

test_that("basic boxplot example works", {
  x <- sonify(iris, sonaes(pitch = Sepal.Length)) + sonfacet(Species) +
    shape_boxplot(length = 1, tempo = 1800)
  soncompare(x, "test-shape-boxplot.wav")
})

