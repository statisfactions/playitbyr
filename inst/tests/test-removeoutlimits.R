set.seed(719)

require(tuneR)

context("remove outside limits")

test_that("removes all events in data that are outside limits on any scaling", {
  x <- sonify(iris[1:10,], sonaes(pitch = Sepal.Width, indx = Petal.Width, time = 1:10)) +
    shape_scatter(jitter = 0.3) + scale_indx_continuous(c(1, 20)) +
      scale_pitch_continuous(c(8, 10), c(3.5, 3.9))
  outfile <- paste(tempfile(), ".wav", sep="")
  sonsave(x,outfile)
  curr <- readWave(outfile)
  unlink(outfile)
  comp <- readWave(system.file("testdata/test-removeoutlimits.wav", package="playitbyr"))
  expect_equal(curr, comp)
})
