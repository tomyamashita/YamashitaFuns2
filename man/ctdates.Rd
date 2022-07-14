\name{ctdates_fun}
\alias{ctdates_fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{CT Table date conversion
%%  ~~function to do ... ~~
}
\description{This function will convert properly formatted date objects to characters. This is so camtrapR can read the character date. For some reason, the package does not like date-formatted dates.
}
\usage{
ctdates_fun(cttable, start.col=6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cttable}{data frame representing a CT Table
%%     ~~Describe \code{x} here~~
}
  \item{start.col}{Defaults to 6. This is the column number in the ct table where dates first appear. It is assumed that all columns after this contain dates.}
}

\value{The same as the input except with character dates instead of date dates.
}

\author{Thomas J. Yamashita
%%  ~~who you are~~
}
\note{As with all of my functions, this assumes a very particular formatting for your data. If the CT table is not formatted in this way, then this function will not work. I would recommend either adjusting your formatting or using this function as a template to build your own. These functions are built for very specific purposes and may not generalize well to whatever you need it for. I build them for my own convenience and make no promises that they will work for different situations. As I come across errors, I attempt to further generalize the function but this is done as I go.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{trapeffort_fun}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
