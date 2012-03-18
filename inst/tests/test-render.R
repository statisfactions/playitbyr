context("general render test")

test_that("basic iris example file works in 'csound'", {
  x <- sonify(iris[11:20,])
  x <- x + sonaes(time=Petal.Length, pitch=Petal.Width)
  x <- x + scale_pitch_continuous(c(6, 8)) + scale_time_continuous(c(0, 10))
  x <- x + shape_scatter(jitter = 0.3)
  soncompare(x, "test-render.wav")
})
