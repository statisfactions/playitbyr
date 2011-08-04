## Print, summary, and addition methods for sonify()
## objects

print.sonify <- function(x, ...) {
  checkSonify(x)
  out <- render(x)
  playAudioRendering(out)
}

summary.sonify <- function(object, ...) {
  mins <- as.vector(lapply(object$scales, function(y) y$min), "character")
  maxs <- as.vector(lapply(object$scales, function(y) y$max), "character")
  firstspaces <- sapply(names(object$scales), function(y) paste(rep(" ",17 - nchar(y)), collapse=""))
  secondspaces <- sapply(mins, function(y) paste(rep(" ",8 - nchar(y)), collapse=""))

  cat((paste("Summary of sonify object '", deparse(substitute(object)), "':\n\n", sep="")))

  cat("The data to be sonified:\n", paste("$dataname \n",
                                          object$dataname, "\n\n"))
  cat("Matchup of sonic values to data columns or constants:\n",
      "      $mapping")
  cat("           Column or Value\n"  )
  cat("--------------------------------------------\n")
  cat(paste("        $", names(object$mapping), " ",
            firstspaces,
            as.vector(object$mapping, "character"),
            sep="", collapse="\n"), "\n\n")

  cat("Desired min/max for sonic parameters:\n",
      "      $scales")
  cat("           Min      Max\n")
  cat("--------------------------------------------\n")
  cat(paste("        $", names(object$scales),
            firstspaces, mins, secondspaces, maxs,
            collapse="\n", sep=""))
  cat("\n")

}

"+.sonify" <- function(x, y) {
  if("sonlayer" %in% class(y)) {
    ## adds sonlayer
    if(is.null(x$sonlayers)) {
      x$sonlayers[[1]] <- y
    } else {x$sonlayers <- c(x$sonlayers, list(y))}
  } else if("sonscaling" %in% class(y)) {
    ## adds to or overrides scale
    for(i in names(x$scales)) {
      if(!is.null(y[[i]])) {
        if(!attr(y[[i]], "default"))
          x$scales[[i]] <- y[[i]]
      }
    }
  } else if("sonaes" %in% class(y)) {
    for(i in names(x$mapping)) {
      if(!is.null(y[[i]])) {
        if(!attr(y[[i]], "default"))
          x$mapping[[i]] <- y[[i]]
      }
    }
    if(is.null(y$time) && !is.null(x$mapping$tempo))
      x$mapping["time"] <- list(NULL)
    if(is.null(y$tempo) & !is.null(x$mapping$time))
      x$mapping["tempo"] <- list(NULL)
  } else if("sonrendering" %in% class(y)) {
    x$rendering <- y
    class(x) <- c(y, class(x)[-1])
  } else {stop("'+' operator not supported for this operation.")}
  x
}         

   


`%+%` <- function(x, y) {
  ##Another possible ggplot2 confusion/conflict
  ##Replace data.frame in x (a sonify object)
  ##with y (data.frame)

  ## This function does not check whether the y's names
  ## match the names in x$mapping, but this is checked
  ## before rendering by checkSonify
  
  x$data <- y
  .checkData(x)
  x$dataname <- deparse(substitute(y))
  x
}
