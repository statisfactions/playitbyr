.onLoad <- function(libname, pkgname) {
  if(is.null(getOption("player")))
    ## We only need to set this option if it is not
    ## already set.
    if(!(Sys.info()[["sysname"]] %in% "Linux")) {
      ## We only set the option if the system is NOT
      ## Linux. audio::play does not work well with Linux.
      options(player = "audio::play")
    } else if(system2("which", "aplay") == 0){
      ## Check to see if aplay is installed if so use it.
      options(player = "aplay")
    }
}
