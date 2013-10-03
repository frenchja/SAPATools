\name{getUSCD}
\alias{getUSCD}
\alias{getACS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to download US census data using the census.gov API.
}
\description{

}
\usage{
getUSCD(x, state='17', key=NULL, delay=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  a list of strings of census variables like: c("H0060001","H0060002")
  }
  \item{state}{
  a list of numeric census state codes like: 17 or c(17,19)
  }
  \item{key}{
  your own census key
  }
  \item{delay}{
  number to seconds to pause btw API hits
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{USCD}{data.frame of the US census data}


}
\references{
\url{http://sapa-project.org/r/}
}
\author{
David M. Condon & Jason A. French <jason at sapa-project.org>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}
\keyword{manip}
