\name{get.sapa}
\alias{get.sapa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.sapa(table.name, con, filename, write = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table.name}{
%%     ~~Describe \code{table.name} here~~
}
  \item{con}{
%%     ~~Describe \code{con} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{write}{
%%     ~~Describe \code{write} here~~
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
%%  ~~who you are~~
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
function (table.name, con, filename, write = TRUE) 
{
    if (!hasArg(con)) {
        message("RMySQL connection not specified. Calling sapa.db() for you.")
        con <- sapa.db()
    }
    if (!hasArg(table.name)) {
        table.choice <- select.list(choices = dbListTables(con), 
            title = "Choose which table to convert to a data.frame: ", 
            multiple = FALSE)
    }
    else {
        tryCatch({
            is.element(el = as.character(table.name), set = dbListTables(con))
            table.choice <- table.name
        }, error = function(e) {
            warning("Invalid table choice!", immediate. = TRUE)
            table.choice <- select.list(choices = dbListTables(con), 
                title = "Choose which table to convert to a data.frame: ")
        })
    }
    data <- dbReadTable(conn = con, name = table.choice)
    if (write == TRUE) {
        table.write <- menu(choices = c("Yes", "No"), title = "Would you like to save your table to disk?")
        write.name <- readline(prompt = "Choose a name for your table in R: ")
        switch(table.write, {
            write.table(x = data, file = paste(write.name, as.character(Sys.Date()), 
                ".data", sep = ""), append = FALSE)
        }, {
            warning("Data not saved to disk yet...")
        })
    }
    return(data)
    on.exit(dbDisconnect(con))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
