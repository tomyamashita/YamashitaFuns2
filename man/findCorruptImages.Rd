\name{findCorruptImages}
\alias{findCorruptImages}
\title{Locate the name of corrupt images
}
\description{A useful, but slow function for locating the name and position of corrupt images within a camera folder.
}
\usage{
findCorruptImages(in.dir, file.type, pp, cores.left = NULL)
}
\arguments{
  \item{in.dir}{String. The directory containing the images that need to be checked. This should lead to only one folder of images.}
  \item{file.type}{String. What type of file should be searched for. This can be any file extension.}
  \item{pp}{Logical. Should the function use parallel processing? This function is extremely slow so parallel processing should help speed things up a bit.}
  \item{cores.left}{Numeric. How many cores should be left available when using parallel processing? The default is NULL. This is only necessary when pp=TRUE. If left at the default, the function will default to 2 cores remaining which is generally enough to continue using a PC while the function runs. I would set this to be greater than 0, otherwise the function will use the entire processing power of your computer. To see how many cores you have available for parallel processing, use: parallel::detectCores().}
}
\details{NOTE: This function is extremely slow. It uses an apply function to run exiftool on each image separately rather than as a batch group, which greatly slows down the process. Parallel processing can help improve this speed but allowing it to check multiple images at once.
}
\value{A list of the corrupt files and the non-corrupt files on either side of it for easy locating within the file directory.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Thomas J. Yamashita}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{cameraRename3}}
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
\keyword{ files }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{ Corrupt Files }
\concept{ Image rename }
% Use only one concept per line.
