\name{sapa.smooth}
\alias{sapa.smooth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Smoothing function for SAPA data}
\description{
Although not necessary for graphics, smoothing the data makes an overall more pleasing map. The smooth function takes a normal “kernel” and applies it to every cell. That is, nearby cells are weighted as a function of the density of the normal curve. The default value of 9 means that the kernel is a 9 x 9 grid. Each grid step is taken to be half a sd of the normal curve. The miss parameter specifies how much to weight cells with no observations. By increasing the weight, we are increasing the tendency to plot average values for missing cells.}
\usage{
sapa.smooth(grid, size = 11, miss = 0.05, w = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{
%%     ~~Describe \code{grid} here~~
}
  \item{size}{
%%     ~~Describe \code{size} here~~
}
  \item{miss}{
%%     ~~Describe \code{miss} here~~
}
  \item{w}{
%%     ~~Describe \code{w} here~~
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
# iq.smooth <-  sapa.smooth(iq.grid,size=11,miss=.05)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
