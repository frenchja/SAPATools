\name{sapa.image}
\alias{sapa.image}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sapa.image(grid, ncols = NULL, main = "SAPA map", database = "state", regions = ".")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{
%%     ~~Describe \code{grid} here~~
}
  \item{ncols}{
%%     ~~Describe \code{ncols} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{database}{
%%     ~~Describe \code{database} here~~
}
  \item{regions}{
%%     ~~Describe \code{regions} here~~
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
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
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
function (grid, ncols = NULL, main = "SAPA map", database = "state", 
    regions = ".") 
{
    if (is.list(grid)) 
        grid <- grid$grid
    map <- map(database = database, plot = FALSE)
    rangex <- map$range[2] - map$range[1]
    rangey <- map$range[4] - map$range[3]
    if (is.null(ncols)) 
        ncols <- ncol(grid$z)
    gr <- colorRampPalette(c("red", "white", "blue"))
    colramp <- gr(ncols)
    image(grid, col = colramp, axes = FALSE, main = main)
    map <- map(database, add = TRUE, regions = regions)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
