context("mappings")

test_that("mapping creates new variable and sonifies it", {
  irisplus <- cbind(iris[1:9,], blah = 1:3)
  x <- sonify(irisplus, sonaes(time = Sepal.Length, pitch = blah)) +
    shape_scatter()
  y <- sonify(irisplus, sonaes(time = Sepal.Length, pitch = 1:3)) +
    shape_scatter()
  outfile1 <- paste(tempfile(), ".wav", sep="")
  set.seed(719)
  sonsave(x, outfile1)
  curr <- readWave(outfile1)
  outfile2 <- paste(tempfile(), ".wav", sep="")
  set.seed(719)
  sonsave(y, outfile2)
  comp <- readWave(outfile2)
  expect_equal(curr, comp)
})

test_that("data mappings can be non-numeric", {
  x <- sonify(iris[41:50,], sonaes(time = Sepal.Length, pitch = Species)) + shape_scatter(jitter = 0.3)
  soncompare(x, "test-nonnumericworks.wav")
})
