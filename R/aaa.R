.onLoad <- function(libname, pkgname) {
}

loadShapeDefs <- function() {
  ## Function to take all shape files--lists saved in separate files
  ## with extension .rda--from the inst/shapeDefs directory and then
  ## load them.

  ## Get file paths of all .rda files with saved shape information
  shapefiles <- dir(system.file("shapeDefs", package="playitbyr"),
                    full.names=T, pattern = "^.*\\.rda$")
  ## and load them
  for(i in shapefiles) load(i)

  ## Remove unused variables so that everything in the current
  ## environment is one of the lists in the shapefiles that we just
  ## loaded
  rm(i, shapefiles)

  ## Coerce current environment to list and return
  allShapes <- as.list(environment())
  return(allShapes)
}

## Actually create the list containing all shapeDefs for use in
## various functions throughout the package
allShapeDefs <- loadShapeDefs()


