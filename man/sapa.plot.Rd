\name{sapa.plot}
\alias{sapa.image}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
sapa.plot}
\description{
Draw image plots of the smoothed or unsmoothed data}
\usage{
sapa.plot(xyz, main = "SAPA map", database = "state", n = 21, range = c(-3, 3))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xyz}{
%%     ~~Describe \code{xyz} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{database}{
%%     ~~Describe \code{database} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{range}{
%%     ~~Describe \code{range} here~~
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
# sapa.plot(iq.inside)
# sapa.image(iq.smooth.inside,main="iq by state",n=51,database="state")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
