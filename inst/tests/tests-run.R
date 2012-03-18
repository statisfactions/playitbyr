require(tuneR)
comppath <- "testdata"

soncompare <- function(x, comppath, seed = 719, basepath = "~/sonification/testdata/") {
  set.seed(seed)
  outfile <- paste(tempfile(), ".wav", sep="")
  sonsave(x,outfile)
  curr <- readWave(outfile)
  unlink(outfile)
  comp <- readWave(paste(basepath, comppath, sep = ""))
  expect_equal(curr, comp)
}

test_file("playitbyr/inst/tests/test-sonopts.R")
test_file("playitbyr/inst/tests/test-mapping.R")
test_file("playitbyr/inst/tests/test-scaleshortcuts.R")
test_file("playitbyr/inst/tests/test-render.R")
test_file("playitbyr/inst/tests/test-setting.R")
test_file("playitbyr/inst/tests/test-shape-scatter.R")
test_file("playitbyr/inst/tests/test-sonify.R")
test_file("playitbyr/inst/tests/test-shape-dotplot.R")
test_file("playitbyr/inst/tests/test-facet.R")
test_file("playitbyr/inst/tests/test-shape-histogram.R")
