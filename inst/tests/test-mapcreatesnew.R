context("mapping tries to create new var when not in dataset")

test_that("mapping creates new variable and sonifies it", {
          irisplus <- cbind(iris[1:9,], blah = 1:3)
          x <- sonify(irisplus, sonaes(time = Sepal.Length, pitch = blah)) +
          shape_scatter()
          y <- sonify(irisplus, sonaes(time = Sepal.Length, pitch = 1:3)) +
          shape_scatter()
          outfile1 <- paste(tempfile(), ".wav", sep="")
          sonsave(x, outfile1)
          curr <- load.wave(outfile1)
          outfile2 <- paste(tempfile(), ".wav", sep="")
          sonsave(y, outfile2)
          comp <- load.wave(outfile2)
          expect_equal(curr, comp)
        })
          



       
