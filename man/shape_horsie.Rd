\name{shape_horsie}
\alias{shape_horsie}
\title{Create a horsie layer}
\usage{
  shape_horsie(jitter = 0, ..., data = NULL,
    mapping = NULL)
}
\arguments{
  \item{jitter}{The maximum size, in seconds, of how much
  to jitter time by when there are multiple notes at the
  same pitch and time (the sonic equivalent of
  overplotting). The default, 0, means no jitter occurs.}

  \item{\dots}{data, settings, and mappings to pass to
  \code{\link{sonlayer}} (see Details)}

  \item{data}{The \code{data.frame} to be sonified for this
  layer. If blank, the data from the parent \code{sonify}
  object is used.}

  \item{mapping}{A \code{\link{sonaes}} object.}
}
\value{
  A \code{sonlayer} object that can be added onto a
  \code{\link{sonify}} object.
}
\description{
  Creates a dotplot layer using the sound of a horse
  instead of the typical snare drum
}
\examples{
x <- sonify(iris[1:10,], sonaes(time = Petal.Length)) + shape_horsie(jitter = 0.3)
\dontrun{print(x)}
}
\author{
  Sound from Freesound user 3bagbrew, licensed
  CC-Attribution.
  \url{http://www.freesound.org/people/3bagbrew/sounds/59569/}
}
\seealso{
  \code{\link{shape_dotplot}}
}

