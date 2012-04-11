require(tuneR)
comppath <- "testdata"

soncompare <<- function(x, comppath, seed = 719, basepath = "~/sonification/testdata/") {
  set.seed(seed)
  outfile <- paste(tempfile(), ".wav", sep="")
  sonsave(x,outfile)
  curr <- readWave(outfile)
  unlink(outfile)
  comp <- readWave(paste(basepath, comppath, sep = ""))
  expect_equal(curr, comp)
}

test_file("test-sonopts.R")
test_file("test-mapping.R")
test_file("test-scaleshortcuts.R")
test_file("test-render.R")
test_file("test-setting.R")
test_file("test-shape-scatter.R")
test_file("test-sonify.R")
test_file("test-shape-dotplot.R")
test_file("test-facet.R")
test_file("test-shape-histogram.R")
test_file("test-play.R")
