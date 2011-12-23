
test_that("all FM + envelope parameters in shape_scatter are exposed", {
  x <- sonify(iris)
  x <- x + sonaes(time=Petal.Length, pitch=Petal.Width, attkp = Sepal.Length,
                  decayp = Sepal.Width, mod = Petal.Length, indx=Petal.Width)
  x <- x + scale_pitch_linear(6, 8) + scale_time_linear(0, 10)
  x <- x + scale_attkp_linear(0.01, 0.3) + scale_decayp_linear(0.01, 3)
  x <- x + scale_mod_linear(0.8, 4) + scale_indx_linear(0, 20)
  x <- x + shape_scatter()
  outfile <- paste(tempfile(), ".wav", sep="")
  sonsave(x, outfile)
  curr <- load.wave(outfile)
  unlink(outfile)
  load(system.file("testdata/csoundirisFM.Rd", package = "playitbyr"))
  expect_equal(curr, comp)
})

