\name{cameraRename}
\alias{cameraRename}
\title{(Deprecated) Rename Camera Trap Images
}
\description{This function will rename camera trap images to date-time and a serial number for each camera folder.

This function has been replaced by the far superior \code{\link{cameraRename2}} function.
}
\usage{
cameraRename(imagedir, exifdir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imagedir}{The directory where the images are stored. If you are using my camera organization methods, this should be the folder containing the camera folder. I doubt this function would work if you do not use this organization method.
%%     ~~Describe \code{x} here~~
  }
  \item{exifdir}{The directory of the exiftool program. The tool can be downloaded from: https://exiftool.org/
This tool is necessary to access the date-time information from camera images. Note that when you download this tool, the executable is named as exiftool(-k).exe. This needs to be renamed to exiftool.exe. If it is not named using this convention, then camtrapR will not be able to locate the tool.

  }
}

\value{No output produced. Files are renamed using the convention "YYYY MM DD HH MM SS XXXXX.JPG" where XXXXX represents a serial number from 1-99999.

}

\author{Thomas J. Yamashita
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{files}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{camera trap images}
% \concept{ ~cpt2 }
% Use only one concept per line.
