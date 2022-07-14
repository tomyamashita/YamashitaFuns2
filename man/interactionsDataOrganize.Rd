\name{interactionsDataOrganize}
\alias{interactionsDataOrganize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create an interactions file from a dataorganize file
%%  ~~function to do ... ~~
}
\description{This function uses the old workflow based on DataOrganize and will create an object for use in computing Wildlife Interactions
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
interactionsDataOrganize(x,y,exclude,start_date,end_date=Sys.Date())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An AllPictures file produced by DataOrganize}
  \item{y}{Environmental variables data frame. This file must have header called "Camera" containing the list of cameras}
  \item{exclude}{species to exclude from the output data frame. Use c() to specify species. If you want keep all items use c(""). Unlike APFun_env, this has no default. Use it to exclude any "species" from the output file}
  \item{start_date}{Start date for the AllPictures file}
  \item{end_date}{End date for pictures. This defaults to the current date}
}
\details{This function is nearly identical to the APFun_env except it is simpler because it does not have to calculate an interval for independent events and it outputs a slightly different object.

For a detailed description of what these interactions are see any of our or UTRGV's reports on the TxDOT camera monitoring projects
}
\value{A file containing the list of images and species for interactions.
}
\references{
Tewes, M., J. Lombardi, Z. Wardle, and T. Yamashita. 2020. Ocelot and Jaguarundi Monitoring Project: Evaluating the Effectiveness of Wildlife Crossings, Cattle Guards, and Fencing on Road Facilities in Cameron County, Contract No. 57-9XXIA003, 0000018485. Feline Research Program, Caesar Kleberg Wildlife Research Institute, Texas A&M University - Kingsville.

Kline, R., K. Ryer, A. Rivera, T. Yamashita, and T. Hopkins. 2020. Post-construction Monitoring Bi-annual Report for SH 100: May 2019 thru October 2019 (Contract No 57-9XXIA001). The University of Texas Rio Grande Valley.
}
\author{Thomas J. Yamashita}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{APFun_env}}
  \code{\link{interactionsTimelapse}}
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
\concept{ DataOrganize }
\concept{ Interactions }
% Use only one concept per line.
