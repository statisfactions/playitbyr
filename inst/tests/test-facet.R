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

test_that("facets for different sonlayers line up", {
  ds <-
structure(list(start = c(0, 0.324859987161938, 0, 0.324859987161938, 
0, 0.324859987161938, 0.580898538460342, 0, 0.324859987161938, 
0.580898538460342, 0, 0.324859987161938, 0.580898538460342), 
    reps = c(1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5)), .Names = c("start", 
"reps"), class = "data.frame", row.names = c("11", "21", "11.1", 
"21.1", "111", "211", "22", "11.11", "21.11", "22.1", "11.2", 
"21.2", "22.2"))
dds <-
structure(list(start = c(0, 0, 0, 0, 0), reps = c(1, 2, 3, 4, 
5)), .Names = c("start", "reps"), class = "data.frame", row.names = c(NA, 
-5L))
x <- sonify() + scale_time_identity() +
  sonfacet(reps, pause = 0) +
  shape_scatter(data = ds, relative = FALSE,
                mapping = sonaes(time = start),  dur = .2) +
  shape_dotplot(data = dds, relative = FALSE,
                mapping = sonaes(time = start), dur = 2)
  soncompare(x, "test-sonfacet2.wav")
})


blah <- function (x, limits, soundlimits) 
  linear_scale(x, limits = NULL, soundlimits, by = eval(by))
