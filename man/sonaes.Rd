\name{sonaes}
\alias{sonaes}
\title{Create mappings and parameters for sonification objects}

\description{
  Creates a \code{sonaes} object, which is a list containing the
  desired mappings of data columns or constants onto each sound
  parameter. This is intended be added to a \code{sonify} object
  (or included in its construction).}

\usage{
sonaes <- function(pitch=NULL, time=NULL, tempo=NULL,
                   dur=NULL, vol=NULL, pan=0.5, timbre="sine")
}

\arguments{
  \item{pitch}{a \code{data.frame} column name (of the "data" argument
    in \code{sonify}), or the desired pitch of all events, represented
    such that 8 represents middle C, 9 represents the octave above,
    etc.}
  \item{time}{a \code{data.frame} column name, or the desired start
    time of all events (in seconds)}
  \item{tempo}{a \code{data.frame} column name, or the desired tempo
    of all events (in beats per minute)}
  \item{dur}{a \code{data.frame} column name, or the desired duration of
    all events (in seconds)}
  \item{vol}{a \code{data.frame} column name, or the desired volume of
    all events (as a proportion of maximum possible amplitude)}
  \item{pan}{a \code{data.frame} column name, or the desired balance
    between left and right stereo channels, where 0 is all the left
    channel, 0.5 balanced, and 1 all the right.}
  \item{timbre}{The desired timbre of the sound. Currently only the
    default value "sine", for a simple sine wave, is supported.}
}

\value{A \code{sonaes} object, used in or added to a \code{sonify}
  object.}

\seealso{
  \code{\link{sonaes}}
}