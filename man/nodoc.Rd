\name{nodoc}
\alias{nodoc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{No documentation created
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addTrack(MIDIOld, MIDITrack, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MIDIOld}{
%%     ~~Describe \code{MIDIOld} here~~
}
  \item{MIDITrack}{
%%     ~~Describe \code{MIDITrack} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (MIDIOld, MIDITrack, ...) 
{
    if (attributes(MIDITrack)$istrack) {
        if (attributes(MIDIOld)$percussion) {
            channelnum <- 10
        }
        else {
            channelnum <- attributes(MIDIOld)$maxchannel + 1
            tracknum <- attributes(MIDIOld)$maxtrack + 1
        }
        MIDIOld[1, 5] <- as.numeric(MIDIOld[1, 5]) + 1
        MIDITrack$tracknum <- tracknum
        MIDITrack$channel <- channelnum
        MIDINew <- rbind(MIDIOld[-nrow(MIDIOld), ], MIDITrack, 
            MIDIOld[nrow(MIDIOld), ])
        class(MIDINew) <- c("MIDI", "data.frame")
        attributes(MIDINew)$istrack <- FALSE
        attributes(MIDINew)$percussion <- FALSE
        attributes(MIDINew)$maxtrack <- attributes(MIDIOld)$maxchannel + 
            1
        attributes(MIDINew)$maxchannel <- attributes(MIDIOld)$maxtrack + 
            1
    }
    else {
        MIDINew <- MIDIOld
        MIDINew[5, 4] <- 6e+07/MIDITrack
    }
    MIDINew
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
