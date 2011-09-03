.onLoad <- function(libname, pkgname) {
  if(is.null(getOption("player")))
    ## We only need to set this option if it is not
    ## already set.
    if(!(Sys.info()[["sysname"]] %in% "Linux")) {
      ## We only set the option if the system is NOT
      ## Linux. audio::play does not work well with Linux.
      options(player = "audio::play")
    } else options(player = "aplay") ## a common CLI wav player

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


