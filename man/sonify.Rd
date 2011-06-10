\name{sonify}
\alias{sonify}
\title{Create sonification objects}
\description{
  Creates a \code{sonify} object, which is a list containing the \code{data.frame}
  to be sonified, the mappings of data to sound parameters, the scaling
  of parameters, and additional options.
}
\usage{
sonify <- function(data=NULL, mapping=sonaes(), scales=scaling(),
                  (sonlayers=shape_notes(), rendering="R"))
}
\arguments{
  \item{data}{a \code{data.frame} with the data to be sonified
  }
  \item{mapping}{a \code{sonaes} object (created by \code{sonaes()})
    that maps data to sound parameters}
  \item{scales}{a \code{sonifyScale} object (created by \code{scaling()})
    that gives the scaling function and desired parameter range from the data to
    each sound}
  \item{sonlayers}{a \code{sonlayer} object. Only the default value, a layer of
    shape type "notes",  is currently supported.}
  \item{rendering}{the method of actually realizing the sound. Only the
    the default value, "audio", to render using the \code{audio}
    package, is currently supported.}
}
\value{
  A \code{sonify} object, which contains what is needed to render the
  object. If the object is completely specified, it can by rendered
  simply by calling the print method for the object (i.e. simply typing
  the name of the object at the R prompt.}

\seealso{
   \code{\link{sonaes}}
}

