\name{removeGhosts}
\alias{removeGhosts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Remove ghosts based on AI
%%  ~~function to do ... ~~
}
\description{Use this function to use the output json file from the Microsoft Megadetector AI to identify images as false captures. It uses a detection threshold derived from the the json file to identify possible ghosts and move them to a user specified directory. The directory has the same structure as the input folder structure.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
removeGhosts(jsonfile, in.dir, out.dir, create.dirs, conf.threshold, move = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{jsonfile}{The filepath to the json file.}
  \item{in.dir}{The directory where the original images are stored. This should be the same directory specified when running the Megadetector AI.}
  \item{out.dir}{The directory where the ghost images should be stored. See details below}
  \item{create.dirs}{Logical. Should R attempt to create sub-directories for the file transfer in the out.dir directory?}
  \item{conf.threshold}{The confidence threshold that should be used to identify ghost images}
  \item{move}{Logical. Defaults to TRUE. Should the images be moved as part of the function. If FALSE, images will not be moved. You can use the output file later to move images using the file.rename function.}
}
\details{It may be beneficial in some cases to not move the images immediately so you can check how many images are actually labelled as ghost. If you want to do this, specify move=F. This is highly recommended as you can confirm that the input file paths are correct.

R will not create directories for you unless you specify create.dirs=T. If you choose to create the file paths yourself and you already have a directory of sorted pictures, you can use Command Prompt in Windows to create the needed folder structure. If you do not have a directory of sorted pictures, you will need to create this manually (or use create.dirs=T):

1. Open command prompt in Windows.

2. Type: xcopy [filepath to the in.dir] [filepath to the out.dir] /t /e

Example: xcopy e:/new_20211015/images e:/new_20211015/ghosts /t /e [Use a "\" instead of a "/" for the file paths]
}
\value{LIST of the input filepaths for each ghost image and the output filepath of each ghost image. If move is specified as FALSE, then this file can be used to move ghost images.
  \item{in.files}{The full file paths to the input ghost images}
  \item{out.files}{The full file paths to the output ghost images}
}

\author{Thomas J. Yamashita
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

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
\concept{ Megadetector }
\concept{ AI }
% Use only one concept per line.
