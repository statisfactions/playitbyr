##' Summary method for sonify objects
##'
##' Summarizes the structure of a sonify object in ways useful for seeing what the output will be.
##'
##' @param object A \code{sonify} object
##' @param \dots Ignored.
##' @return \code{summary.sonify} prints a brief summary, including 
##' info about the dataset to be sonified, and the specified mappings and
##' scalings of sonic values to data parameters.
##' 
##' @seealso \code{\link{sonify}} for the creation of these objects
##' @method summary sonify
##' @author Closely based on \code{summary.ggplot} by Hadley Wickham
##' @examples
##' summary(sonify(iris, sonaes(time = Sepal.Length, pitch = Sepal.Width)) +
##'         shape_scatter() + shape_dotplot())
##' @export
summary.sonify <- function(object, ...) {
  wrap <- function(x) paste(
                        paste(strwrap(x, exdent = 2), collapse = "\n"),
                        "\n", sep =""
                        )
  
  defaults <- function() {
    paste(mapply(function(x, n) {
      paste(n, deparse(x), sep="=")
    }, object$mapping, names(object$mapping)), collapse=", ")
  }

  clist <- function(l) {
    paste(paste(names(l), l, sep=" = ", collapse=", "), sep="")
  }

  datamap <- function(x) {
    if (!is.null(x$data)) {
      output <- paste(
                  "data:     ", paste(names(x$data), collapse=", "), 
                  " [", nrow(x$data), "x", ncol(x$data), "] ", 
                  "\n", sep="")
      cat(wrap(output))
    }
    if (length(x$mapping) > 0) {
      cat("mapping:  ", clist(x$mapping), "\n", sep="")    
    }
  }

  datamap(object)
  cat("faceting: ")
  print(object$sonfacet)
  ## TODO act more gracefully for arbitrary sonlayers without shape specified
  if (length(object$sonlayers) > 0) {
    cat("-----------------------------------\n")
    lapply(object$sonlayers, function(x) {
      cat("shape_", x$shape$shape, ":  ", clist(x$shape$shape_params), "\n", sep="")
      datamap(x)
    })
    cat("-----------------------------------\n")
    cat("minimum and maximum values of shape parameters:\n\n")
    scales <- .getScales(object)
    minmaxes <- do.call(rbind, (lapply(scales, function(y) y$soundlimits)))
    colnames(minmaxes) <- c("Min", "Max")
    print(minmaxes)
    invisible(NULL)
  }
  else
    cat("No layers present\n")
}
