require(audio)

context("setting vs mapping")

test_that("setting accepted outside of sonaes()", {
          x <- sonify(iris[1:10,], sonaes(time = Sepal.Width)) + shape_scatter(pitch = 8)
          outfile <- paste(tempfile(), ".wav", sep="")
          sonsave(x,outfile)
          curr <- load.wave(outfile)
          unlink(outfile)
          load(system.file("testdata/setmap.rda", package="playitbyr"))
          expect_equal(curr, comp)
        })
