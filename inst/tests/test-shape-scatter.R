set.seed(719)

require(tuneR)


test_that("all FM + envelope parameters in shape_scatter are exposed", {
  x <- sonify(iris[21:30,])
  x <- x + sonaes(time=Petal.Length, pitch=Petal.Width, attkp = Sepal.Length,
                  decayp = Sepal.Width, mod = Petal.Length, indx=Petal.Width)
  x <- x + scale_pitch_continuous(c(6, 8)) + scale_time_continuous(c(0, 10))
  x <- x + scale_attkp_continuous(c(0.01, 0.3)) + scale_decayp_continuous(c(0.01, 3))
  x <- x + scale_mod_continuous(c(0.8, 4)) + scale_indx_continuous(c(0, 20))
  x <- x + shape_scatter(jitter = 0.3)
  soncompare(x, "test-shape-scatter.wav")
})

test_that("relative = FALSE works", {
  x <- sonify(iris[40:50,]) + sonaes(time = Petal.Length) +
    shape_scatter(relative = FALSE, dur = 2.6)
  expect_true(all(.getScore(x)[[1]]$dur == 2.6))
})

test_that("shape_scatter examples work", {
x <- sonify(iris, sonaes(time = Petal.Width, pitch = Petal.Length)) +
  shape_scatter() # no jitter
soncompare(x, "test-shape-scatter1.wav")
x <- sonify(iris, sonaes(time = Petal.Width, pitch = Petal.Length)) +
  shape_scatter(jitter = 0.3) # substantial jitter, fuzzes out overlap
soncompare(x, "test-shape-scatter2.wav")
## relative = TRUE: rescales duration to fit overall length (usually easier to hear)
d <- cbind(airquality, row = rownames(airquality))
x <- sonify(d, sonaes(time = row, pitch = Temp)) + shape_scatter(dur = 3) +
  scale_time_continuous(c(0, 10))
soncompare(x, "test-shape-scatter3.wav")
x <- sonify(d, sonaes(time = row, pitch = Temp)) + shape_scatter(dur = 3) +
  scale_time_continuous(c(0, 5))
soncompare(x, "test-shape-scatter4.wav")
## relative = FALSE: duration is in seconds and is not scaled to fit overall length
## (creates lots of overlap)
x <- sonify(d, sonaes(time = row, pitch = Temp)) + shape_scatter(relative = FALSE, dur = 3) +
  scale_time_continuous(c(0, 10))
soncompare(x, "test-shape-scatter5.wav")
x <- sonify(d, sonaes(time = row, pitch = Temp)) + shape_scatter(relative = FALSE, dur = 3) +
  scale_time_continuous(c(0, 5))
soncompare(x, "test-shape-scatter6.wav")
})










