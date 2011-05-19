render.csound <- function(x) {
  ##option setting should be made more portable, and moved to a separate file!
  ##.Tcl calls should be changed to (safer) tcl calls and passing of tcl objects
  require(tcltk)
  sco <- df.notes(x)
  sco$inst <- match(sco$timbre, c("drum", "sine"))
  sleeptime <- ceiling(sco$start[nrow(sco)] + sco$dur[nrow(sco)] + 0.5)

  sco <- sco[c("inst", setdiff(names(sco), c("sonlayer", "timbre", "inst")))] ## not currently supported here
  
  out <- paste("csNote",do.call(paste, sco)) 

  orcfile <- system.file("inst/templates/playitbyrinst.orc", pkg="playitbyr")

  .Tcl(paste("load", getOption("tclcsound.path")))
  if(!(.csCompiled)) {
    .Tcl(paste("csCompile", getOption("csound.plain"), orcfile))
    .csCompiled <- TRUE
  } else {.Tcl("csRewind")}

    sapply(out, .Tcl)

    .Tcl("csPlay")

  print("\n ############## \n Note: Csound is still running.\n Type 'csStop()' to stop running Csound. \n")
}


csStop <- function() .Tcl("csStop")
