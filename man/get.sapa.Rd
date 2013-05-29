\name{get.sapa}
\alias{get.sapa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get SAPA Project data from MySQL
}
\description{
This function uses sapa.table(), sapa.db(), and clean.sapa() to read MySQL tables
and output full data.frames in R.}
\usage{
data <- get.sapa(date)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date}{
Minimum date to obtain data from (YYYY-MM-DD).
}
  \item{filename}{
Filename to use if writing to disk.
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
Jason A. French <frenchja@u.northwestern.edu>
David M. Condon <emailcondon@gmail.com>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (date) 
{
    if (hasArg(date)) {
        print(paste("Obtaining SAPA date from ", date))
        date <- as.Date(date, format = "\%Y-\%m-\%d")
    }
    else {
        warning("No date supplied.  Using 05/20/2013!")
        date <- as.Date("2013-5-20", format = "\%Y-\%m-\%d")
    }
    sapa.active.list <- c("TAIE_responses_052013", "CAI_responses_052013", 
        "emotion_responses_052013", "health_responses_052013", 
        "peer_responses_052013")
    con.active <- sapa.db(database = con.active)
    lapply(X = sapa.active, FUN = function(x) {
        x <- sapa.table(sapa.table = x, con = con.active)
    })
    dbDisconnect(con.active)
    if (date < as.Date("2013-5-20", format = "\%Y-\%m-\%d")) {
        sapa.archive.list <- c("")
        lapply(X = sapa.archive.list, FUN = function(x) {
        })
    }
    data$time <- as.Date(data$time, format = "\%Y-\%m-\%d \%H:\%M:\%S")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
