\name{sapa.db}
\alias{sapa.db}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Connect to SAPA database
}
\description{
This function establshes a connection to the specified MySQL database
using the RMySQL package.}
\usage{
sapa.db(database, all = FALSE)
}
\arguments{
  \item{database}{
Choose the SAPA database (i.e., 'SAPAactive' or 'SAPAarchive')
}
  \item{all}{
Logical for establishing connections for all databases
}
}
\details{
http://sapa-project.org/r/}
\value{
\item{con}{RMySQL Connection to SAPA}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jason A. French \email{jason at sapa-project.org}}


\seealso{
}
\examples{
# con <- sapa.db(database='SAPAactive')
}


