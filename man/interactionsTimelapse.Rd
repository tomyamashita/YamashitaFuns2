\name{interactionsTimelapse}
\alias{interactionsTimelapse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create an Interactions File from a Timelapse Output
}
\description{This function copies images for interactions to a new folder and creates an interactions file
}
\usage{
interactionsTimelapse(images, envdata, exclude, create.dirs=T, copy.files=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{images}{A timelapse csv, formatted using our Timelapse template}
  \item{envdata}{Environmental variables data frame. This file must have header called "Camera" containing the list of cameras, "Site" containing the site name, "Side" indicating which side the camera is on, and "Type" indicating what type of structure the camera is at.}
  \item{exclude}{species to exclude from the output data frame. Use c() to specify species. If you want keep all items use c(""). Unlike APFun_env, this has no default. Use it to exclude any "species" from the output file}
  \item{create.dirs}{Logical. Defaults to TRUE. Should new directories be checked for and created by R if necessary?}
  \item{copy.files}{Logical. Defaults to TRUE. Should image files be copied to the appropriate directories?}
}
\details{NOTE: R has no good way of handling duplicate file names so it will skip duplicates when copying. Depending on how images are named, this could be an issue. The easiest way to check is to check the number of images in the main folder
}
\value{A list containing the data file and a file of the old and new paths and names
  \item{Interactions}{The Interactions data file}
  \item{Files}{File used for copying files}
}
\references{
Tewes, M., J. Lombardi, Z. Wardle, and T. Yamashita. 2020. Ocelot and Jaguarundi Monitoring Project: Evaluating the Effectiveness of Wildlife Crossings, Cattle Guards, and Fencing on Road Facilities in Cameron County, Contract No. 57-9XXIA003, 0000018485. Feline Research Program, Caesar Kleberg Wildlife Research Institute, Texas A&M University - Kingsville.

Kline, R., K. Ryer, A. Rivera, T. Yamashita, and T. Hopkins. 2020. Post-construction Monitoring Bi-annual Report for SH 100: May 2019 thru October 2019 (Contract No 57-9XXIA001). The University of Texas Rio Grande Valley.
}
\author{Thomas J. Yamashita
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{interactionsDataOrganize}}
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
\concept{ Timelapse }
\concept{ Interactions }
% Use only one concept per line.
