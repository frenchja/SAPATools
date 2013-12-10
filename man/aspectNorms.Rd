\name{AspectNorms}
\alias{aspectNorms}
\alias{aspect.norms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Recalculate the Mean and SD for the 12 aspect scores
}
\description{
This function is an omnibus function to obtain, clean, score, and build the 
SAPA R data.frame used by the Telemetrics Lab at Northwestern University.}
\usage{
AspectMatrix <- AspectNorms(x)
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{aspectMatrix }{Matrix of means and sds for 12 aspect scores.}

}
\references{
\url{https://sapa-project.org/r/}
}
\author{
David M. Condon and Jason A. French <jason at sapa-project.org>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Produces matrix of normed Aspect scores from SAPA data.frame
# AspectNorms(TAIE)
}

