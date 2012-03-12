set.seed(719)

require(tuneR)

context("setting vs mapping")

test_that("setting accepted outside of sonaes()", {
          x <- sonify(iris[1:10,], sonaes(time = Sepal.Width)) + shape_scatter(pitch = 8)
          outfile <- paste(tempfile(), ".wav", sep="")
          sonsave(x,outfile)
          curr <- readWave(outfile)
          unlink(outfile)
          comp <- readWave(system.file("testdata/test-settingmapping.wav", package="playitbyr"))
          expect_equal(curr, comp)
        })
