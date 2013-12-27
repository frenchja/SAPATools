\name{sapa.clean}
\alias{sapa.clean}
\title{
sapa.clean}
\description{sapa.clean
}
\usage{
sapa.clean(data, database = "usa", DV = "g")
}
\arguments{
  \item{data}{
}
  \item{database}{
}
  \item{DV}{
}
}

\references{
https://sapa-project.org/r/
}
\author{
William Revelle <revelle@northwestern.edu>
Maintainer:  Jason A. French
}
\note{
Please file bugs at https://github.com/frenchja/SAPATools/issues.
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, database = "usa", DV = "g") 
{
    map <- map(database, plot = FALSE)
    rangex <- map$range[2] - map$range[1]
    rangey <- map$range[4] - map$range[3]
    clean <- subset(data, (data[, "longitude"] > map$range[1]))
    clean <- subset(clean, (clean[, "longitude"] < map$range[2]))
    clean <- subset(clean, (clean[, "latitude"] > map$range[3]))
    clean <- subset(clean, (clean[, "latitude"] < map$range[4]))
    result <- data.frame(longitude = clean[, "longitude"], latitude = clean[, 
        "latitude"], DV = clean[, DV])
    return(result)
  }
}


