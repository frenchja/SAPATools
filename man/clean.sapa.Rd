\name{clean.sapa}
\alias{clean.sapa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scrub SAPA of duplicate participants and bad ages
}
\description{
Scrubs SAPA of duplicate participants.
Also returns a clean SAPA dataframe containing ages within min.age and max.age.  
}
\usage{
clean.sapa(x, max.age = 91, min.age = 13, age=TRUE, unique=TRUE)
}

\arguments{
  \item{x}{
SAPA data.frame to scrub
}
  \item{max.age}{
Maximum age desired}
  \item{min.age}{
Minimum age desired
}
 \item{age}{
Logical to scrub bad ages}
 \item{unique}{
Logical to scrub duplicate participants
}
}
\details{
}
\value{
\item{data.frame}{Returned SAPA data.frame}

}
\references{
http://sapa-project.org/r}
\author{
Jason A. French \email{jason at sapa-project.org
David M. Condon

Maintainer:  Jason A. French}

\examples{
# con.active <- sapa.db()

# sapa.table <- get.sapa(con=con.active)

# clean.table <- clean.sapa(sapa.table)
}

