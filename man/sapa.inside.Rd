\name{sapa.inside}
\alias{sapa.inside}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
sapa.inside}
\description{
Converts the grid based data to a list of x, y, and z coordinates that is used by the sapa.plot function.}
\usage{
sapa.inside(grid, database = "usa", regions = ".")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{
%%     ~~Describe \code{grid} here~~
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
http://sapa-project.org/r/}
\author{
William Revelle <revelle@northwestern.edu>
Maintainer:  Jason A. French
}
\note{
Please file bugs at https://github.com/frenchja/SAPATools/issues.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# iq.smoothed.inside <- sapa.inside(iq.smooth)
# iq.inside <- sapa.inside(iq.grid$average)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
