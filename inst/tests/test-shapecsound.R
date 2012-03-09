context("shape_csound")

test_that("shape_csound works!", {
  i1 <- t(matrix(c(1, 0, 22, .5, 1)))
    i34 <- c(inst = 34, dur = 0.5, 30000, 8, .5, .5, .2, 1, .5, .5, 1, 1.5)
    names(i34)[-(1:2)] <- paste("p", 4:(length(i34) + 1), sep = "")
    x <- sonify(iris[1:10,], sonaes(start = Sepal.Width)) + do.call(shape_csound, as.list(i34)) + sonscaling(start = list(0, 20, linear_scale))
    x <- x + sonopts(i = list(i1), orcfile = "/home/fortis/sonification/tmpcsound/drums2.orc")
    out <- tempfile()
    sonsave(x, out)
    curr <- load.wave(out)
    unlink(out)
    load(system.file("testdata/shapecsound.rda", package = "playitbyr"))
    expect_equal(curr, comp)
  })
    

    
