\name{check.location}
\alias{check.location}
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
\arguments{
  \item{ssh.user}{Your NetID used to establish an SSH tunnel to the SAPA Project. 
  See \code{\link{sapa.db}} for more information}
}
\details{
https://sapa-project.org/r/}
\value{
  \item{TRUE}{Returns TRUE if run from server or an SSH connection is established.}
}
\references{
https://sapa-project.org/r}
\author{
Jason A. French \email{jason@sapa-project.org}
}


\seealso{
\code{\link{get.sapa}},
\code{\link{make.sapa},
\code{\link{merge.sapa}},
\code{\link{sapa.db}},
}}

\examples{
# Establish an SSH tunnel if necessary.
# check.location(ssh.user='NetID')
}

