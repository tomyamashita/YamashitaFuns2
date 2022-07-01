\name{timelapseQC}
\alias{timelapseQC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Quality control for timelapse
%%  ~~function to do ... ~~
}
\description{This function outputs the unique species from a timelapse file, a list of rows that were not sorted by species, and a list of rows that were not sorted by individual. You should use this to check whether a set of images was completely sorted.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
timelapseQC(ds)
}
\arguments{
  \item{ds}{A data frame created from a csv file exported from Timelapse. Note, this function is set up to use my timelapse template.
%%     ~~Describe \code{x} here~~
}
}
\details{This function will check if there are video files (.MP4) then check the number of unique species, whether any image was not sorted by species by checking if the species1 column contains any "" values, and whether any image was not sorted by individuals by checking if a column sorted into a species was not sorted by individual. Ghosts, humans, "", birds, rodents, lizards, and amphibians are not sorted by individual under the current protocol, and are therefore not included in this check. Video files are also not checked by individual.

    Once the timelapse file has passed this check, it can be used in the standard workflow for images.
}
\value{LIST containing unique species and data frames of missing species files and missing individuals files
  \item{Unique Species}{String. The list of the unique species identified from the file}
  \item{Missing Species}{Data frame. Any entries that were not sorted by species is included in this file.}
  \item{Missing Ind}{Data frame. Any entries that should have been but were not sorted by individual are included here.}
}

\author{Thomas J. Yamashita
%%  ~~who you are~~
}

\seealso{
  \code{\link{APFun_Timelapse}}
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
\keyword{ debugging }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
