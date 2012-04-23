context("boxplot")

test_that("basic boxplot example works", {
  x <- sonify(iris, sonaes(pitch = Sepal.Length)) + sonfacet(Species) +
    shape_boxplot(length = 1, tempo = 1800)
  soncompare(x, "test-shape-boxplot.wav")
    x <- sonify(iris, sonaes(pitch = Sepal.Length)) +
    shape_boxplot(length = 2, tempo = 1800) # plays the same thing for longer
  soncompare(x, "test-shape-boxplot1.wav")
x <- sonify(iris, sonaes(pitch = Sepal.Length)) +
  shape_boxplot(length = 1, tempo = 1200) #  same length as original but fewer pitches
soncompare(x, "test-shape-boxplot2.wav")
})

