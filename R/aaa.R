.onLoad <- function(libname, pkgname) {
if(is.null(getOption("audioRendering")))
    if(Sys.info()[["sysname"]] %in% "Linux") {
    options(audioRendering = "tempfile")} else {
        options(audioRendering = "audio::play")
      }
}
