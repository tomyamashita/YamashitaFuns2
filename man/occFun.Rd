\name{occFun}
\alias{occFun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Occupancy Analysis Data Setup
%%  ~~function to do ... ~~
}
\description{This function modifies data for use in occupancy modelling using Unmarked or a separate program, I think.
}
\usage{
occFun(x, ct, unit, subset, stationCol, sessionCol=NULL, ct_probs=T, count=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data frame produced by the APFun_env function available in this package}
  \item{ct}{A CT Table following the format from the package camtrapR}
  \item{unit}{The temporal unit for dividing up your data. This must be in units "days"}
  \item{subset}{A character vector indicating which species to prepare data for}
  \item{stationCol}{Character. A character string for the column used to identify sites}
  \item{sessionCol}{Character. A character string for the column used to identify sessions. This is currently untested. Use with caution.}
  \item{ct_probs}{Logical. Does the CT table have problems?}
  \item{count}{Logical. Should presence/absence (FALSE) or counts (TRUE) be outputted?}
}
\details{Please note that I personally have never tested the output of this function with an occupancy model although I am told that it works. Please use this with caution and test it properly before using it for any real research.
}
\value{A LIST object by species chosen in the subset input with a table of sites (rows) and dates (columns)
}

\author{Code author: Thomas J. Yamashita

Testing and error checking: Aidan B. Branney
}

\seealso{
  \code{\link{APFun_env}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ datagen }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{ Occupancy Modelling }
\concept{ DataOrganize }
\concept{Camera Trapping}
% Use only one concept per line.
