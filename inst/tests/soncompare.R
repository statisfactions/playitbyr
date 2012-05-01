## utility function for running these tests
soncompare <<- function(x, comppath, seed = 719, basepath = "./") {
  set.seed(seed)
  outfile <- paste(tempfile(), ".wav", sep="")
  sonsave(x,outfile)
  curr <- readWave(outfile)
  unlink(outfile)
  comp <- readWave(paste(basepath, comppath, sep = ""))
  expect_equal(curr, comp)
}

