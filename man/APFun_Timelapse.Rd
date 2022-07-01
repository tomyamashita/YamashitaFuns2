\name{APFun_Timelapse}
\alias{APFun_Timelapse}
\title{Convert a Timelapse csv to a format for use with APFun_env
}
\description{This function converts a Timelapse csv file to the dataorganize file output format. This was done this way because many of my core functions for processing camera trap data depend on the existence of a data frame created by APFun_env. Converting a timelapse file to this format is just the easiest way to maintain consistency.
}
\usage{
APFun_Timelapse(x)
}
\arguments{
  \item{x}{A Timelapse csv file}
}

\value{An R object formatted in the same style as a dataorganize text file.}

\author{Thomas J. Yamashita
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
\concept{ Camera Trapping }
\concept{ Timelapse }
% Use only one concept per line.
