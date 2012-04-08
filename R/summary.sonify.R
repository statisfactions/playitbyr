##' Summary method for sonify objects
##'
##' Summarizes the structure of a sonify object in ways useful for seeing what the output will be.
##'
##' @param object A \code{sonify} object
##' @param \dots Ignored.
##' @return \code{summary.sonify} prints a brief summary of the name
##' of the dataset to be sonified, and the specified mappings and
##' scalings of sonic values to data parameters.
##' 
##' @seealso \code{\link{sonify}} for the creation of these objects
##' @method summary sonify
##' @export
summary.sonify <- function(object, ...) {
  mins <- as.vector(lapply(object$scales, function(y) y$min), "character")
  maxs <- as.vector(lapply(object$scales, function(y) y$max), "character")
  firstspaces <- sapply(names(object$scales), function(y) paste(rep(" ",17 - nchar(y)), collapse=""))
  secondspaces <- sapply(mins, function(y) paste(rep(" ",8 - nchar(y)), collapse=""))

  cat((paste("Summary of sonify object '", deparse(substitute(object)), "':\n\n", sep="")))

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

