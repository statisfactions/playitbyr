context("general render test")

test_that("basic iris example works in 'audio'", {
  x <- sonify(iris, opts = sonopts("audio", samp.rate = 1000))
  x <- x + sonaes(time=Petal.Length, pitch=Petal.Width)
  x <- x + scale_pitch_linear(6, 8) + scale_time_linear(0, 10)
  x <- x + shape_scatter()
  curr <- create_audioSample(x)
  load(system.file("testdata/audioiris.Rd", package="playitbyr"))
  expect_equal(curr, comp)
})
