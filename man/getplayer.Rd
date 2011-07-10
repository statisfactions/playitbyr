\name{getPlayer}
\alias{getPlayer}
\alias{setPlayer}
\usage{
getPlayer()
setPlayer(player)
}
\title{Get and set wave file player}
\description{This allows you to set the external wav file player when you are
  rendering to a temp file. I recommend a command-line player to avoid
  having to wait for a GUI to load up.
}
\arguments{
  \item{player}{A string containing the path to the wav file player.}
}
\value{
  \code{getPlayer()} returns the string set by \code{setPlayer()}
  }
