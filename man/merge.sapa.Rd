\name{get.sapa}
\alias{get.sapa}
\title{
Get SAPA Project data from MySQL
}
\description{
This function uses get.sapa(), sapa.db(), and clean.sapa() to read and 
merge SAPA MySQL tables 
and output full data.frames in R.}
\usage{
data <- get.sapa(date)
}
\arguments{
  \item{date}{
Minimum date to obtain data from (YYYY-MM-DD).
}
  \item{filename}{
Filename to use if writing to disk.
}
}
\details{
http://sapa-project.org/r/}
\value{
\item{data}{Merged data.frame}

}
\references{
}
\author{
Jason A. French <frenchja@u.northwestern.edu>
David M. Condon <emailcondon@gmail.com>
}
\note{
}


\seealso{
}
\examples{
combined.data <- merge.sapa()
}