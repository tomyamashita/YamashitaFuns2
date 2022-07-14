\name{movePictures}
\alias{movePictures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Move pictures from unsorted to sorted folders
%%  ~~function to do ... ~~
}
\description{This function uses a Timelapse csv file to move or copy images from an unsorted folder to sorted folders based on species and number of individuals (in the same format as required for \code{link{dataorganize}}), although see details.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
movePictures(x, in.dir, out.dir, create.dirs, type = "none")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Data Frame. A data frame of a timelapse file. Note, this should follow the timelapse template that I typically use}
  \item{in.dir}{String. The directory containing the unsorted images}
  \item{out.dir}{String. The directory where you want to store the sorted images}
  \item{create.dirs}{Logical. Should the function create the directories it needs?}
  \item{type}{String. Should you move, copy, or do nothing with the images. Choose one of c('move','copy','none')}
}
\details{
When this function creates its folder structure, it uses the Individuals column in the Timelapse output. For some "species" (e.g., ghost, human, bird, rodent), we do not sort these by individual, therefore the Individuals column is a 0. These species get assigned a folder of 00 for their number of individuals. I do not know how this will affect quality control and the workflow down the line. Generally, it should not be an issue but could result in NA values in the Individuals column of the \code{\link{APFun_env}} or errors in the \code{\link{dataorganize}} functions in this package. Once this has been tested, I will update this.
}

\value{list of the full file path to the in files and out files
  \item{in.files}{String. Full file paths to the in files}
  \item{out.files}{String. Full file paths to the out files}
}

\author{Thomas J. Yamashita}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{dataorganize}}
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
\keyword{ manip }
\keyword{ misc }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{ Megadetector }
\concept{ AI }
\concept{ Folder structure }
% Use only one concept per line.
