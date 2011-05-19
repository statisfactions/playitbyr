
.First.lib <- function(libname, pkgname) {
  if(is.null(getOption("csvmidi"))) option(csvmidi = "csvmidi")
  if(is.null(getOption("csound.plain")))
  ## This snipped taken from Erich Neuwirths GPL Rcsound pacage
  {
    options(csound.plain = "-odevaudio -m0")
    options(csound.debug = "-odevaudio -m15")
  }

if(is.null(getOption("tclcsound.path")))
  options(tclcsound.path="/usr/lib/tclcsound/tclcsound.so")


  .csCompiled <<- FALSE
  .csStopped <<- TRUE
}

  
