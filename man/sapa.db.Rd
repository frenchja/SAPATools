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
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{database}{
Choose the SAPA database (i.e., 'SAPAactive', 'SAPAcurrent', or 'SAPAarchive')
}
  \item{all}{
Logical for establishing connections for all databases
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
%% ~put references to the literature/web site here ~
}
\author{
Jason A. French <frenchja@u.northwestern.edu>}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
con <- sapa.db(SAPAactive)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

