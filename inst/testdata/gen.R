require(audio)

## csoundiris.Rd
  x <- sonify(iris)
  x <- x + sonaes(time=Petal.Length, pitch=Petal.Width)
  x <- x + scale_pitch_continuous(6, 8) + scale_time_continuous(0, 10)
  x <- x + shape_scatter()
sonsave(x, "blah.wav")
##system2("aplay", "blah.wav")
comp <- load.wave("blah.wav")
save(comp, file = "playitbyr/inst/testdata/csoundiris.rda")

## csoundirisFM.Rd
  x <- sonify(iris)
  x <- x + sonaes(time=Petal.Length, pitch=Petal.Width, attkp = Sepal.Length,
                  decayp = Sepal.Width, mod = Petal.Length, indx=Petal.Width)
  x <- x + scale_pitch_continuous(6, 8) + scale_time_continuous(0, 10)
  x <- x + scale_attkp_continuous(0.01, 0.3) + scale_decayp_continuous(0.01, 3)
  x <- x + scale_mod_continuous(0.8, 4) + scale_indx_continuous(0, 20)
  x <- x + shape_scatter()
sonsave(x, "blah.wav")
comp <- load.wave("blah.wav")
##system2("aplay", "blah.wav")
save(comp, file = "playitbyr/inst/testdata/csoundirisFM.rda")

## nonnumeric.rda
  x <- sonify(iris, sonaes(time = Sepal.Length, pitch = Species)) + shape_scatter()
  outfile <- paste(tempfile(), ".wav", sep="")
  sonsave(x, outfile)
sonsave(x, "blah.wav")
comp <- load.wave("blah.wav")
##system2("aplay", "blah.wav")
save(comp, file = "playitbyr/inst/testdata/nonnumeric.rda")

## setmap.Rd
          x <- sonify(iris[1:10,], sonaes(time = Sepal.Width)) + shape_scatter(pitch = 8)
          outfile <- paste(tempfile(), ".wav", sep="")
sonsave(x, "blah.wav")
comp <- load.wave("blah.wav")
##system2("aplay", "blah.wav")
save(comp, file = "playitbyr/inst/testdata/setmap.rda")

unlink("blah.wav")


