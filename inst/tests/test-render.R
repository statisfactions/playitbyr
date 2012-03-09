set.seed(719)

require(audio)

context("general render test")

test_that("basic iris example file works in 'csound'", {
  x <- sonify(iris)
  x <- x + sonaes(time=Petal.Length, pitch=Petal.Width)
  x <- x + scale_pitch_continuous(6, 8) + scale_time_continuous(0, 10)
  x <- x + shape_scatter()
  outfile <- paste(tempfile(), ".wav", sep="")
  sonsave(x,outfile)
  curr <- load.wave(outfile)
  unlink(outfile)
  load(system.file("testdata/csoundiris.rda", package="playitbyr"))
  expect_equal(curr, comp)
})
