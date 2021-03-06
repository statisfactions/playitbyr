\name{sonify}
\alias{sonify}
\title{Create sonification objects}
\usage{
  sonify(data = NULL, mapping = sonaes(),
    scales = sonscaling(), sonlayers = NULL,
    opts = sonopts(), sonfacet = NULL)
}
\arguments{
  \item{data}{a \code{data.frame} with the data to be
  sonified}

  \item{mapping}{This specifies the mappings of the data
  paramters (i.e. columns of \code{data}) to sound
  parameters (such as start time, duration, et cetera).
  It's a \code{sonaes} object, best created by
  \code{\link{sonaes}}.

  The available sound parameters for mapping depend on what
  kind of sonification you're doing. This is specified by
  the \code{shape_} convenience function, and the available
  sound parameters are documented on those help pages (see
  e.g. \code{\link{shape_scatter}}).}

  \item{scales}{What range of the sound parameter should
  the data be scaled to, and how should the scaling be
  done? Practically, the best way to deal with tweaking
  scales is to add on the \code{scale_} convenience
  function for whichever sound parameter you want to
  change, like \code{\link{scale_time_continuous}}.

  If specifying manually (not recommended), this is a
  \code{\link{sonscaling}} object which gives the scaling
  function and desired parameter range from the data to
  each sound.}

  \item{sonlayers}{Layers allow you to specify the type of
  audio display, and multiple layers allow you to display
  different aspects of the data to play simultaneously. The
  best way of doing this is to add on the \code{shape_}
  function that you want. Current possibilities are:

  \describe{

  \item{\code{\link{shape_scatter}}}{A scatterplot of notes
  with many aspects of the sound available for setting and
  mapping, generated by FM synthesis}

  \item{\code{\link{shape_dotplot}}}{Creates a sense of the
  distribution of a variable, represented by start time,
  using a snare drum sound}

  \item{\code{\link{shape_histogram}}}{Gives a sense of the
  distribution of values (represented by pitch) by sampling
  the dataset (this can be used along with faceting to
  facilitate comparisons of groups).}

  \item{\code{\link{shape_boxplot}}}{Gives a sense of the
  distribution of values (represented by pitch) by sampling
  the dataset in increasingly narrow percentiles around the
  median}

  \item{\code{\link{shape_csound}}}{A layer that can use an
  arbitrary csound file as its orchestra (not for
  beginners!)} }

  If specifying manually (not recommended), this is a
  single \code{sonlayer} object, or a list of
  \code{sonlayer} objects, that gives each layer to be
  rendered.}

  \item{opts}{Advanced rendering options; this is easiest
  to use by adding on the function \code{\link{sonopts}}}

  \item{sonfacet}{\pkg{playitbyr} supports faceting--this
  splits \code{data} by the values in one of its columns
  and creates separate sonifications for each group, played
  on after another. See \code{\link{sonfacet}} for more
  information on how to do this.

  If specifying manually (not recommended), this is a
  \code{\link{sonfacet}} object that gives what variables
  to split by.}
}
\value{
  A \code{sonify} object, which contains what is needed to
  render the object. If the object is completely specified,
  it can by rendered simply by calling the print method for
  the object (i.e. simply typing the name of the object at
  the R prompt).
}
\description{
  Creates a \code{sonify} object, the fundamental workhorse
  of \pkg{playitbyr}; this is a list containing the
  \code{data.frame} to be sonified, the mappings of data to
  sound parameters, the scaling of parameters, and
  additional options.
}
\details{
  The easiest way of building up a sonification is to
  create a \code{sonify} object with this function,
  specifying the \code{data.frame} you want sonified as the
  \code{data} parameter and possibly the mappings. Then,
  you can add on (with \code{+}) additional mappings,
  layers, scalings and facets later. See Arguments section
  for more explanation of each of these pieces, and the
  Examples for how you can add the pieces together to form
  a complete sonification.
}
\note{
  When used in a loop, function, or other non-interactive
  use, a \code{sonify} object needs to be printed in order
  to actually be turned into sound. So, assign the output
  of this function to an object and then use
  \code{print(foo)} to hear the sonification.
}
\examples{
## A bare-bones sonify object, but one that cannot render
x <- sonify()
summary(x)
\dontrun{x # Throws an error message}

## Creating a sonification incrementally
## and with the "scale_" convenience functions
x <- sonify(iris)
x <- x + sonaes(time=Petal.Length, pitch=Petal.Width)
x <- x + shape_scatter()
x <- x + scale_pitch_continuous(c(6, 8)) + scale_time_continuous(c(0, 10))
summary(x)
\dontrun{x # outputs sound}
}
\references{
  The syntax of \code{sonify} objects is heavily based on
  that of \code{ggplot} objects in the \pkg{ggplot2}
  package. See the \href{http://had.co.nz/ggplot2/}{ggplot2
  web site} for more information.
}
\seealso{
  \code{\link{summary.sonify}}
}

