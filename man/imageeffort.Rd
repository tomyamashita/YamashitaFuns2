\name{imageeffort}
\alias{imageeffort_fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Camera trapping effort (Images)
%%  ~~function to do ... ~~
}
\description{This function calculates the total number of pictures, number of animals, ghosts, and humans from one or more timelapse or dataorganize files
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
imageeffort_fun(x, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{List. A list object containing either timelapse files or dataorganize files. If you name your files, the names will be outputted in the result.}
  \item{type}{String. What was the source of the input files. Choose one of c("timelapse", "dataorganize").}
}

\details{If a timelapse file is given to the function, it will run the \code{\link{APFun_Timelapse}} function to convert it to a dataorganize file.
}

\value{A data frame containing total number of pictures, number of pictures of animals, ghosts, and humans, and the success rate for animal pictures in each file added as well as a row for the total number of pictures.
}

\author{
Thomas J. Yamashita
}
\note{As with all of my functions, this assumes a very particular formatting for your data. I would recommend either adjusting your formatting or using this function as a template to build your own. These functions are built for very specific purposes and may not generalize well to whatever you need it for. I build them for my own convenience and make no promises that they will work for different situations. As I come across errors, I attempt to further generalize the function but this is done as I go.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{dataorganize}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
\concept{ Image Processing }
\concept{ Camera Effort }
% Use only one concept per line.
