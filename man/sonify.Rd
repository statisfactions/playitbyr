\name{sonify}
\alias{sonify}
\title{Create sonification objects}
\description{
  Creates a \code{sonify} object, which is a list containing the \code{data.frame}
  to be sonified, the mappings of data to sound parameters, the scaling
  of parameters, and additional options.
}
\usage{
sonify(data=NULL, mapping=sonaes(), scales=sonscaling())
}
\arguments{
  \item{data}{a \code{data.frame} with the data to be sonified
  }
  \item{mapping}{a \code{sonaes} object (created by \code{sonaes()})
    that maps data to sound parameters}
  \item{scales}{a \code{sonifyScale} object (created by \code{scaling()})
    that gives the scaling function and desired parameter range from the data to
    each sound}
  % PLANNED BUT NOT IMPLEMENTED YET
  % \item{sonlayers}{a \code{sonlayer} object. Only the default value, a layer of
  %   shape type "notes",  is currently supported.}
  % \item{rendering}{the method of actually realizing the sound. Only the
  %   the default value, "audio", to render using the \code{audio}
  %   package, is currently supported.}
}
\value{
  A \code{sonify} object, which contains what is needed to render the
  object. If the object is completely specified, it can by rendered
  simply by calling the print method for the object (i.e. simply typing
  the name of the object at the R prompt).}

\seealso{
  
  \code{\link{+.sonify}} for interacting with \code{sonify} objects;
  \code{\link{sonaes}} for setting the mappings;
  \code{\link{sonscaling}} and \code{\link{scaleShortcuts}} for creating
  the scalings.

}
\references{
  The syntax of \code{sonify} objects is heavily based on that of
  \code{ggplot} objects in the \pkg{ggplot2} package. See the
  \href{http://had.co.nz/ggplot2/}{ggplot2 web site} for more
  information.}

\examples{

## A bare-bones sonify object, but one that cannot render
x <- sonify()
summary(x)
\dontrun{x # Throws an error message}

## A more complete specification
## for a sonify object that uses iris
## with "Petal.Width" mapped onto pitches
## between pitch values 6 (110 Hz) and 8 (440 Hz),
## and "Petal.Length" mapped onto times
## between 0 and 10 seconds.
x <- sonify(data=iris,
            sonaes(pitch="Petal.Width",
                   time="Petal.Length"),
            sonscaling(pitch=list(6, 8, linear.scale),
                       time=list(0, 10, linear.scale)))
summary(x)
\dontrun{x # outputs sound}

## You can hear there is a cluster of low values at the beginning
## when both Petal.Width and Petal.Length are small,
## followed by a higher cluster.

## The same, created incrementally
## and with the "scale_" convenience functions
x <- sonify(data=iris)
x <- x + sonaes(pitch="Petal.Width", time="Petal.Length")
x <- x + scale_pitch_linear(6, 8) + scale_time_linear(0, 10)
summary(x)
\dontrun{x # outputs sound}
}
