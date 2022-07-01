\name{timeConvert}
\alias{timeConvert}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert Time Zones from GPS collar data
%%  ~~function to do ... ~~
}
\description{
This function converts date-time information from a GPS collar from UTC time to your local time zone. This should be used when GPS collar data is not already converted and you need to keep separate "date" and "time" columns in your data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
timeConvert(ds, date.col, time.col, LocalTZ)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ds}{The original GPS collar data}
  \item{date.col}{The column containing the original dates that need to be converted}
  \item{time.col}{The column containing the original times that need to be converted}
  \item{LocalTZ}{The time zone that you want to convert to. This must an R-recognized string for a given time zone. This function utilizes the with_tz function from the lubridate package. See the help for that package for details.}
}

\value{Your original data frame with the new date and time columns appended to the end as LocalDate and LocalTime
}

\author{Thomas J. Yamashita, Aidan B. Branney
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{lubridate::with_tz}}
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
\keyword{ manip }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{ Collars }
% \concept{ ~cpt2 }
% Use only one concept per line.
