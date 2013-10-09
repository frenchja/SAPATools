\name{SAPATools-package}
\alias{SAPATools-package}
\alias{SAPATools}
\alias{SAPA Tools}
\alias{SAPA}
\docType{package}
\title{
R Tools for use with the SAPA Project
}
\description{
SAPA Tools is an R package developed by Jason A. French and David M. Condon for using the SAPA dataset.  
SAPA is an open-source personality collaboratory in which participants receive a random subset of items, 
leading to a matrix in which the data are massively missing at random (MMCAR).
}
\details{
\tabular{ll}{
Package: \tab SAPATools\cr
Type: \tab Package\cr
Version: \tab 2013.10.09\cr
Date: \tab 2013-10-09\cr
License: \tab GPL (>= 2)\cr
}
SAPA Tools is an R package developed by Jason A. French and David M. Condon for using the SAPA dataset. As such, relevant functions are included for:

    Obtaining the dataset from MySQL
    Scrubbing the dataset of duplicate entries
    Applying factor names to the various SAPA demographic codes
    Scoring open-source cognitive ability items from icar-project.org
    Integrating US census data with SAPA variables

We think the SAPA Project is an incredible resource for researchers for learning about personality using traits, abilities, and interests. As such, the primary goal of SAPA Tools is to make it easy to play with our data. 
}
\author{
Jason A. French \email{jason at sapa-project.org}

Maintainer: Jason A. French \email{jason at sapa-project.org}
}
\references{
French, J. A. & Condon, D. M. (2013). SAPA Tools: Tools to analyze the SAPA Project. R package version 2013.10.04. http://sapa-project.org/r.

Wilt, J., Condon, D., & Revelle, W. (2012). Telemetrics and online data collection. Handbook of Developmental Research Methods, 163-179.

\url{http://sapa-project.org/r/}
}

\seealso{
\link{psych}
}
\examples{
# Connect to the SAPA database
# con.active <- sapa.db()

# Pull a single table for analysis
# mytable <- get.sapa(con='con.active')

# Get and scrub all the current SAPA data
# mydata <- clean.sapa()
}