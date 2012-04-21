set.seed(719)

require(tuneR)

context("setting vs mapping")

test_that("setting accepted outside of sonaes()", {
  x <- sonify(iris[1:10,], sonaes(time = Sepal.Width)) + shape_scatter(jitter = 0.3, pitch = 8)
  soncompare(x, "test-settingmapping.wav")
  ## doc examples
  x <- sonify(iris[1:10,], sonaes(time = Sepal.Width)) + shape_scatter(pitch = 9)
  soncompare(x, "test-settingmapping2.wav")
  x <- sonify(iris[1:10,], sonaes(time = Sepal.Width, pitch = 9)) + shape_scatter()
  soncompare(x, "test-settingmapping3.wav")
})
