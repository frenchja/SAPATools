\name{make.sapa}
\alias{make.sapa}
\alias{build.sapa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Build and export SAPA data.frame
}
\description{
This function is an omnibus function to obtain, clean, score, and build the 
SAPA R data.frame used by the Telemetrics Lab at Northwestern University.}
\usage{
make.sapa(file)
}

\value{
\item{data.frame}{Combined SAPA data.frame}

}
\references{
https://sapa-project.org/r}
\author{
Jason A. French \email{jason at sapa-project.org} and David M. Condon
}


\examples{
# Produces sapa.CURRENTDATE.rdata in your current directory
#make.sapa()

# Produces chosen file and pathname
#make.sapa(file='~/sapa.rdata')
}
