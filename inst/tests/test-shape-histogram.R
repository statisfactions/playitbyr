context("histogram")

test_that("basic histogram example works", {
  x <- sonify(iris, sonaes(pitch = Sepal.Length)) + sonfacet(Species) +
    shape_histogram(length = 3, tempo = 1800)
  soncompare(x, "test-shape-histogram.wav")
  x <- sonify(iris, sonaes(pitch = Sepal.Length)) + sonfacet(Species) +
    shape_histogram(length = 5, tempo = 1800) # plays the same thing for longer
  soncompare(x, "test-shape-histogram1.wav")
  x <- sonify(iris, sonaes(pitch = Sepal.Length)) + sonfacet(Species) +
    shape_histogram(length = 3, tempo = 1200) #  same length as original but fewer pitches
  soncompare(x, "test-shape-histogram2.wav")
})

