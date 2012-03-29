require(tuneR)

context("play wav through csound")

test_that("rendering is real time by default", {
  expect_true(getOption("render_real_time"))
})

test_that("sonsave play out is same as sonsave output", {
  x <- sonify(iris[11:20,])
  x <- x + sonaes(time=Petal.Length, pitch=Petal.Width)
  x <- x + scale_pitch_continuous(c(6, 8)) + scale_time_continuous(c(0, 10))
  x <- x + shape_scatter(jitter = 0.3)
  outfile <- paste(tempfile(), ".wav", sep="")
  playout <- paste(tempfile(), ".wav", sep="")
  sonsave(x, outfile, play = TRUE, out = playout)
  son <- readWave(outfile)
  playcomp <- readWave(playout)
  unlink(outfile)
  unlink(playout)
  expect_equal(son, playcomp)
})
