\name{sapa.clean}
\alias{sapa.clean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
sapa.clean}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sapa.clean(data, database = "usa", DV = "g")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{database}{
%%     ~~Describe \code{database} here~~
}
  \item{DV}{
%%     ~~Describe \code{DV} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

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


