set.seed(719)

require(tuneR)


test_that("all FM + envelope parameters in shape_scatter are exposed", {
  x <- sonify(iris[21:30,])
  x <- x + sonaes(time=Petal.Length, pitch=Petal.Width, attkp = Sepal.Length,
                  decayp = Sepal.Width, mod = Petal.Length, indx=Petal.Width)
  x <- x + scale_pitch_continuous(c(6, 8)) + scale_time_continuous(c(0, 10))
  x <- x + scale_attkp_continuous(c(0.01, 0.3)) + scale_decayp_continuous(c(0.01, 3))
  x <- x + scale_mod_continuous(c(0.8, 4)) + scale_indx_continuous(c(0, 20))
  x <- x + shape_scatter()
  outfile <- paste(tempfile(), ".wav", sep="")
  sonsave(x, outfile)
  curr <- readWave(outfile)
  unlink(outfile)
  comp <- readWave(system.file("testdata/test-shape-scatter.wav", package = "playitbyr"))
  expect_equal(curr, comp)
})

