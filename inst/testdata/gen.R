require(tuneR)

set.seed(719)
x <- sonify(iris[11:20,])
x <- x + sonaes(time=Petal.Length, pitch=Petal.Width)
x <- x + scale_pitch_continuous(c(6, 8)) + scale_time_continuous(c(0, 10))
x <- x + shape_scatter()
sonsave(x, "playitbyr/inst/testdata/test-render.wav")

set.seed(719)
x <- sonify(iris[21:30,])
x <- x + sonaes(time=Petal.Length, pitch=Petal.Width, attkp = Sepal.Length,
                decayp = Sepal.Width, mod = Petal.Length, indx=Petal.Width)
x <- x + scale_pitch_continuous(c(6, 8)) + scale_time_continuous(c(0, 10))
x <- x + scale_attkp_continuous(c(0.01, 0.3)) + scale_decayp_continuous(c(0.01, 3))
x <- x + scale_mod_continuous(c(0.8, 4)) + scale_indx_continuous(c(0, 20))
x <- x + shape_scatter()
sonsave(x, "playitbyr/inst/testdata/test-shape-scatter.wav")

set.seed(719)
x <- sonify(iris[41:50,], sonaes(time = Sepal.Length, pitch = Species)) + shape_scatter()
sonsave(x, "playitbyr/inst/testdata/test-nonnumericworks.wav")

set.seed(719)
x <- sonify(iris[1:10,], sonaes(time = Sepal.Width)) + shape_scatter(pitch = 8)
sonsave(x, "playitbyr/inst/testdata/test-settingmapping.wav")

set.seed(719)
## dotplot.rda
x <- sonify(iris[1:10,], sonaes(time = Petal.Length)) + shape_dotplot()
sonsave(x, "playitbyr/inst/testdata/test-dotplot.wav")

set.seed(719)
x <- sonify(iris[1:10,], sonaes(pitch = Sepal.Width, indx = Petal.Width, time = 1:10)) +
  shape_scatter() + scale_indx_continuous(c(1, 20)) +
  scale_pitch_continuous(c(8, 10),c(3.5, 3.9))
sonsave(x, "playitbyr/inst/testdata/test-removeoutlimits.wav")


