##' Get and set the audio player
##' 
##' This allows you to get set the method used to play the result of
##' your sonification. These functions are probably only necessary on
##' Linux, but work on Linux, OS X, and Windows.
##' 
##' By default on Windows and OS X, audio is played with the \code{play}
##' function from the \pkg{audio} package. So \code{getPlayer()} will return
##' \code{"audio::play"}.
##' 
##' However, \pkg{audio}'s \code{play} function does not work well on
##' Linux--for instance, it seems to cause segmentation faults--so the default
##' is \href{http://linux.die.net/man/1/aplay}{aplay}, which comes along with
##' many Linux distributions. If you want to use another player, you can use
##' \code{setPlayer()} to specify an external wav file player. Then, when
##' \code{print}-ing a \code{sonify} object, \pkg{playitbyr} will render to a
##' temporary file and then play it using \code{\link{system2}}.
##' 
##' For external players, I recommend a command-line player to avoid having to
##' wait for a GUI to load up.
##' 
##' @rdname getPlayer
##' @aliases getPlayer setPlayer
##' @param newPlayer A character string; either \code{"audio::play"} to
##' indicate that you are using the \code{play} function from the \pkg{audio}
##' package, or the path to media (\code{*.wav}) file player. See Details.
##' @return \code{setPlayer()} is called for its side effect, to set the option
##' for \code{player}.
##' 
##' \code{getPlayer()} returns the option set by \code{setPlayer()}.
##' @seealso \code{\link{print.sonify}}, \code{\link{playLastRendering}}
##' @examples
##' 
##' ## Get the current player
##' oldPlayer <- getPlayer()
##' oldPlayer
##' 
##' ## Set the current player to aplay
##' setPlayer("aplay")
##' getPlayer()
##' 
##' ## Reset player to the old setting
##' setPlayer(oldPlayer)
##'
##' @export
##' @usage getPlayer()
getPlayer <- function() getOption("player")

##' @rdname getPlayer
##' @export
setPlayer <- function(newPlayer) options(player = newPlayer)

