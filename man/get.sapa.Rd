\name{get.sapa}
\alias{get.sapa}
\title{
get.sapa
}
\description{
Imports a single Synthetic Aperture Personality Assessment (i.e., SAPA) 
table from MySQL into R.}
\usage{
get.sapa(table.name, con, filename, write = TRUE)
}

\arguments{
  \item{table.name}{
Desired MySQL table in quotes (e.g., 'TAI...').
}
  \item{user}{SAPA MySQL username}
  \item{password}{SAPA MySQL password}
  \item{con}{Existing RMySQL connection.  Output object of sapa.db()}
  \item{filename}{If write=TRUE, the desired filename to write.}
  \item{write}{Write the output of get.sapa() to an .Rdata file?}
}
\details{
See https://sapa-project.org/r/}
\value{
  \item{data}{data.frame from MySQL table.}
}
\references{
https://sapa-project.org/r/}
\author{
Jason A. French \email{jason@sapa-project.org}
}

\seealso{
\code{\link{merge.sapa}}
\code{\link{sapa.db}}
\code{\link{make.sapa}}
\code{\link{check.location}}
}
\examples{
# con.active <- sapa.db('SAPAactive')
# TAIdata <- get.sapa(table.name='TAI_responses_052013',con=con.active)
}