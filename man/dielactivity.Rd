\name{DielActivity}
\alias{actfun}
\title{ Animal Diel Activity
}
\description{A function for calculating diel activity for selected species. The function can optionally also split your data based on a user-defined column.
}
\usage{
actfun(x, split=F, splitcol=NULL, species, bw = NULL, rep = 999)
}
\arguments{
  \item{x}{Data frame. This should be the output from running the APFun_env function.
  }
  \item{split}{Logical. Whether you want to split your data by a column in the data frame.
  }
  \item{splitcol}{(Optional). Defaults to NULL. Which column to use to split the data. Only required if split=T.
  }
  \item{species}{Character. Which species do you want to run diel activity on? Use form c("Species 1", "Species 2", "etc.")
  }
  \item{bw}{Numeric or NULL. Do you want to specify a bandwidth for the kernel density estimation of the activity distribution? The default is NULL where the function with calculate bandwidth internally.
  }
  \item{rep}{Numeric. The number of bootstraps that should be used to estimate the confidence intervals of the diel activity distribution.
  }
}
\details{As with all of my functions, this assumes a very particular formatting for your data. If the CT table is not formatted in this way, then this function will not work. I would recommend either adjusting your formatting or using this function as a template to build your own. These functions are built for very specific purposes and may not generalize well to whatever you need it for. I build them for my own convenience and make no promises that they will work for different situations. As I come across errors, I attempt to further generalize the function but this is done as I go.
}
\value{
  \item{data}{These are the data tables for each species (and split) used to produce the activity distribution.
  }
  \item{activity}{These are the estimations of diel activity for each species.
  }
}
\references{Rowcliffe, J. M., R. Kays, B. Kranstauber, C. Carbone, and P. A. Jansen. 2014. Quantifying levels of animal activity using camera trap data. Methods in Ecology and Evolution 5:1170-1179. 10.1111/2041-210x.12278

Frey, S., J. T. Fisher, A. C. Burton, and J. P. Volpe. 2017. Investigating animal activity patterns and temporal niche partitioning using camera-trap data: challenges and opportunities. Remote Sensing in Ecology and Conservation 3:123-132. 10.1002/rse2.60

Ramesh, T., R. Kalle, K. Sankar, and Q. Qureshi. 2012. Spatio-temporal partitioning among large carnivores in relation to major prey species in Western Ghats. Journal of Zoology 287:269-275. 10.1111/j.1469-7998.2012.00908.x
}
\author{
Thomas J Yamashita
}

\seealso{
https://cran.r-project.org/web/packages/activity/index.html
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
\keyword{univar}
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{Diel Activity}
% \concept{ ~cpt2 }
% Use only one concept per line.
