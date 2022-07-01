\name{dataorganize}
\alias{dataorganize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{DataOrganize
%%  ~~function to do ... ~~
}
\description{This is an R version of the DataOrganize program developed by Jim Sanderson and Grant Harris. While untested, it should provide a little more flexibility in naming of folders than the original DataOrganize program. It also can do basic diagnostics so you can check camera and species names.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dataorganize(inputdir, diagnostics = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputdir}{The directory containing the camera folders.}
  \item{diagnostics}{Logical. Should diagnostic information be outputted to the console? This is set to TRUE by default.}
}

\value{A data frame organized in the same way as the DataOrganize program: Camera Species Year Month Day Hour Minute Second Num_of_Individuals
}
\references{Original DataOrganize program: https://smallcats.org/resources/
%% ~put references to the literature/web site here ~
}
\author{Thomas J. Yamashita
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{APFun_env}}
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
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
