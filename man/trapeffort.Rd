\name{trapeffort}
\alias{trapeffort_fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Camera trapping effort
%%  ~~function to do ... ~~
}
\description{This function calculates the number of active camera trap nights and total camera trap nights using an inputted CT table, formatted based on camtrapR specifications.Required columns are setup date and retrieval date and theoretically, the table should have problems. I have never tested it on a dataset without any problems.
}
\usage{
trapeffort_fun(cttable, group, sessions=F, sessioncol)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cttable}{data frame}
  \item{group}{A column in the CT Table indicating how camera trap nights should be calculated. Generally accepted are c("Camera", "Site", "Station"). }
  \item{sessions}{Logical. Does your data have multiple sessions (e.g., field seasons, etc.)? This defaults to FALSE}
  \item{sessioncol}{(Optional). What column distinguishes sessions? This should not be needed if sessions is set to F. If you have problems, set this to NULL. }
}
\details{Make sure that your CT table is formatted properly. That is the only way this function works. Also, camptrapR for some reason removed its support for dates in "Date" or "POSIXct" format so dates must be in character format. You can use my ctdates_fun function to fix this in a CT table.
}
\value{A data frame containing the items from the group column, session column (if included), active camera trap nights, and total camera trap nights.
}

\author{Thomas J. Yamashita}

\note{As with all of my functions, this assumes a very particular formatting for your data. If the CT table is not formatted in this way, then this function will not work. I would recommend either adjusting your formatting or using this function as a template to build your own. These functions are built for very specific purposes and may not generalize well to whatever you need it for. I build them for my own convenience and make no promises that they will work for different situations. As I come across errors, I attempt to further generalize the function but this is done as I go.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{ctdates_fun}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ manip }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{ Camera Trapping }
% \concept{ ~cpt2 }
% Use only one concept per line.
