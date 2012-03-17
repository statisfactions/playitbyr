context("facet")

test_that("basic faceting works as expected", {
  df <- data.frame(pits = c(0, 1, 1, 2, 2, 3),
                   tims = c(0, 1, 0, 1, 0, 1),
                   facs = c(0, 0, 1, 1, 2, 2))
  x <- sonify(df, sonaes(time = tims, pitch = pits)) + shape_scatter(dur = 1) +
    scale_time_continuous(c(0, 1)) + scale_pitch_continuous(c(8, 8.25)) +
      sonfacet(facs, pause = 1)
  y <- sonify(df, sonaes(time = tims, pitch = pits)) + shape_scatter(dur = 1, jitter = 0.5) +
    scale_time_continuous(c(0, 1)) + scale_pitch_continuous(c(8, 8.25)) +
      sonfacet(facs, pause = 1)
  soncompare(x, "test-facet.wav")
  soncompare(y, "test-facet.wav")
})

