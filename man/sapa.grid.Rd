\name{sapa.grid}
\alias{sapa.grid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a longitude by latitude grid of the data
}
\description{
Creates a grid of the means for each zip code for a particular Dependent Variable (e.g., ’g’). Data that are in the same grid are averaged. Note that the larger the grid size, the more precise the results, but the time taken for this and subsequent calculations increase by the square of the grid dimensions. There is an ’average’ option which subtracts the overall weighted average from every cell. This functionally centers the data around that average. The alternative is to have all empty cells have the average.}
\usage{
sapa.grid(df, DV, gridsize = 300, database = "state", average = TRUE, regions = ".")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{DV}{
%%     ~~Describe \code{DV} here~~
}
  \item{gridsize}{
%%     ~~Describe \code{gridsize} here~~
}
  \item{database}{
%%     ~~Describe \code{database} here~~
}
  \item{average}{
%%     ~~Describe \code{average} here~~
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
https://sapa-project.org/r/}
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
# iq.grid <- sapa.grid(data=s.zip,DV='g',grid=300,average=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
