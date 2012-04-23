oldwd <- getwd()
setwd("testdata")

set.seed(719)
x <- sonify(iris[11:20,])
x <- x + sonaes(time=Petal.Length, pitch=Petal.Width)
x <- x + scale_pitch_continuous(c(6, 8)) + scale_time_continuous(c(0, 10))
x <- x + shape_scatter(jitter = 0.3)
sonsave(x, "test-render.wav")

set.seed(719)
x <- sonify(iris[21:30,])
x <- x + sonaes(time=Petal.Length, pitch=Petal.Width, attkp = Sepal.Length,
                decayp = Sepal.Width, mod = Petal.Length, indx=Petal.Width)
x <- x + scale_pitch_continuous(c(6, 8)) + scale_time_continuous(c(0, 10))
x <- x + scale_attkp_continuous(c(0.01, 0.3)) + scale_decayp_continuous(c(0.01, 3))
x <- x + scale_mod_continuous(c(0.8, 4)) + scale_indx_continuous(c(0, 20))
x <- x + shape_scatter(jitter = 0.3)
sonsave(x, "test-shape-scatter.wav")

set.seed(719)
x <- sonify(iris[41:50,], sonaes(time = Sepal.Length, pitch = Species)) + shape_scatter(jitter = 0.3)
sonsave(x, "test-nonnumericworks.wav")

set.seed(719)
x <- sonify(iris[1:10,], sonaes(time = Sepal.Width)) + shape_scatter(jitter = 0.3, pitch = 8)
sonsave(x, "test-settingmapping.wav")

set.seed(719)
x <- sonify(iris[1:10,], sonaes(time = Petal.Length)) + shape_dotplot(jitter = 0.3)
sonsave(x, "test-dotplot.wav")

set.seed(719)
x <- sonify(iris[1:10,], sonaes(pitch = Sepal.Width, indx = Petal.Width, time = 1:10)) +
  shape_scatter(jitter = 0.3) + scale_indx_continuous(c(1, 20)) +
  scale_pitch_continuous(c(8, 10), c(3.5, 3.9))
sonsave(x, "test-removeoutlimits.wav")

df <- data.frame(pits = c(0, 1, 1, 2, 2, 3),
           tims = c(0, 1, 0, 1, 0, 1),
           facs = c(0, 0, 1, 1, 2, 2))
x <- sonify(df, sonaes(time = tims, pitch = pits)) + shape_scatter(dur = 1) +
  scale_time_continuous(c(0, 1)) + scale_pitch_continuous(c(8, 8.25)) +
  sonfacet(facs, pause = 1)
sonsave(x, "test-facet.wav")

set.seed(719)
x <- sonify(iris, sonaes(pitch = Sepal.Length)) + sonfacet(Species) +
    shape_histogram(length = 3, tempo = 1800)
sonsave(x, "test-shape-histogram.wav")

set.seed(719)
x <- sonify(iris, sonaes(pitch = Sepal.Length)) + sonfacet(Species) +
  shape_boxplot(length = 1, tempo = 1800)
sonsave(x, "test-shape-boxplot.wav")

set.seed(719)
x <- sonify(iris[1:10,], sonaes(time = Sepal.Width)) + shape_scatter(pitch = 9)
sonsave(x, "test-settingmapping2.wav")

set.seed(719)
x <- sonify(iris[1:10,], sonaes(time = Sepal.Width, pitch = 9)) + shape_scatter()
sonsave(x, "test-settingmapping3.wav")

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
sonsave(x, "test-sonfacet2.wav")

x <- sonify(iris, sonaes(time = Petal.Width, pitch = Petal.Length)) +
  shape_scatter() # no jitter
sonsave(x, "test-shape-scatter1.wav")
set.seed(719)
x <- sonify(iris, sonaes(time = Petal.Width, pitch = Petal.Length)) +
  shape_scatter(jitter = 0.3) # substantial jitter, fuzzes out overlap
sonsave(x, "test-shape-scatter2.wav")

## relative = TRUE: rescales duration to fit overall length (usually easier to hear)
d <- cbind(airquality, row = rownames(airquality))
x <- sonify(d, sonaes(time = row, pitch = Temp)) + shape_scatter(dur = 3) +
  scale_time_continuous(c(0, 10))
sonsave(x, "test-shape-scatter3.wav")
x <- sonify(d, sonaes(time = row, pitch = Temp)) + shape_scatter(dur = 3) +
  scale_time_continuous(c(0, 5))
sonsave(x, "test-shape-scatter4.wav")

## relative = FALSE: duration is in seconds and is not scaled to fit overall length
## (creates lots of overlap)
x <- sonify(d, sonaes(time = row, pitch = Temp)) + shape_scatter(relative = FALSE, dur = 3) +
  scale_time_continuous(c(0, 10))
sonsave(x, "test-shape-scatter5.wav")
x <- sonify(d, sonaes(time = row, pitch = Temp)) + shape_scatter(relative = FALSE, dur = 3) +
  scale_time_continuous(c(0, 5))
sonsave(x, "test-shape-scatter6.wav")

setwd(oldwd)
