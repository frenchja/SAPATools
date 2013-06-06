\name{check.location}
\alias{check.location}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
check.location
}
\description{
Ensures that user is running package from SAPA Server.  If not, optionally uses 
an SSH tunnel to access SAPA MySQL database.  Used by other functions to reduce 
errors.}
\usage{
check.location()
}
%- maybe also 'usage' for other objects documented here.
\details{
http://sapa-project.org/r/}
\value{
  \item{TRUE}{Returns TRUE if run from server or an SSH connection is established.}
}
\references{
http://sapa-project.org}
\author{
Jason A. French <frenchja@u.northwestern.edu>}
\note{
}

\section{Warning }{....}

\seealso{
\code{\link{get.sapa}},
\code{\link{make.sapa},
\code{\link{merge.sapa}},
\code{\link{sapa.db}},
}
\examples{
check.location()
}

