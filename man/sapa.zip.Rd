\name{sapa.zip}
\alias{sapa.zip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
sapa.zip
}
\description{
The sapa.zip takes a data file (e.g., IRT.scores) and combines it with the zip codes from the zip code database. The output is a data.frame of the means and ns of the original variables by zip code.
}
\usage{
sapa.zip(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data.frame
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
# s.zip <- sapa.zip(IRT.scores)
}

