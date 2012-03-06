context("general render test")

test_that("basic iris example works in 'audio'", {
  x <- sonify(iris, opts = sonopts("audio", samp.rate = 1000))
  x <- x + sonaes(time=Petal.Length, pitch=Petal.Width)
  x <- x + scale_pitch_continuous(6, 8) + scale_time_continuous(0, 10)
  x <- x + shape_scatter()
  curr <- create_audioSample(x)
  load(system.file("testdata/audioiris.Rd", package="playitbyr"))
  expect_equal(curr, comp)
})

test_that("basic iris example file works in 'csound'", {
  x <- sonify(iris)
  x <- x + sonaes(time=Petal.Length, pitch=Petal.Width)
  x <- x + scale_pitch_continuous(6, 8) + scale_time_continuous(0, 10)
  x <- x + shape_scatter()
  outfile <- paste(tempfile(), ".wav", sep="")
  sonsave(x,outfile)
  curr <- load.wave(outfile)
  unlink(outfile)
  load(system.file("testdata/csoundiris.Rd", package="playitbyr"))
  expect_equal(curr, comp)
})
