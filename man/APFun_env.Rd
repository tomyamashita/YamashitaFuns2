\name{APFun}
\alias{APFun_env}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{All Pictures Function with Environmental Variables for a DataOrganize output
%%  ~~function to do ... ~~
}
\description{Function for combining an AllPictures text file created by DataOrganize with environmental variables associated with individual camera traps. The No Interval version of the function uses an interval of 1 second and does not recalculate # number of individuals
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
APFun_env(x,y,sort.col="Camera",exclude=c("ghost"),start_date,
          end_date=Sys.Date(),interval=NULL,all.pics=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{AllPictures file produced by DataOrganize}
  \item{y}{Environmental variables data frame. This file must have header called "Camera" containing the list of cameras}
  \item{sort.col}{Column that you want to sort pictures by to create independent events. This should be one of c("Camera", "Site", "Station") depending on your wording for sites. This defaults to "Camera"}
  \item{exclude}{species to exclude from the output data frame. Use c() to specify species. This defaults to excluding ghosts. If you want keep all items use c("")}
  \item{start_date}{Start date for the AllPictures file}
  \item{end_date}{End date for pictures. This defaults to the current date}
  \item{interval}{Time in seconds between pictures for an independent event. See Details for some normal interval times. This is only necessary for the main function. It defaults to no interval}
  \item{all.pics}{Logical. Whether or not you want to return all the pictures or just independent events. If all.pics = TRUE, all pictures will be returned with an index number associated with independent events. This defaults to FALSE}
}
\details{Suggested interval times: 61 = 1 minute, 1801 = 30 minutes. 3601 = 1 hour
One second should be added to the total to ensure the full interval is included
%%  ~~ If necessary, more details than the description above ~~
}
\value{A data frame of camera data. Output varies depending on whether or not all.pics is true or false.
  \item{all.pics = FALSE}{Returns a data frame of only independent events at the specified interval}
  \item{all.pics = TRUE}{Returns a data frame of all pictures from the input file. A index number is added associated with independent events}
}
\author{Thomas J. Yamashita, Kevin Ryer
}
\note{As with all of my functions, this assumes a very particular formatting for your data. If the EnvData table is not formatted in this way, then this function will not work. I would recommend either adjusting your formatting or using this function as a template to build your own. These functions are built for very specific purposes and may not generalize well to whatever you need it for. I build them for my own convenience and make no promises that they will work for different situations. As I come across errors, I attempt to further generalize the function but this is done as I go.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{APFun_Timelapse}}
  \code{\link{dataorganize}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ datagen }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{ DataOrganize }
\concept{ AllPictures }
% Use only one concept per line.
