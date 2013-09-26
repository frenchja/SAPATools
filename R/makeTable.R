makeTable <- function (x) {
	# Author:  David M. Condon
	# args:
	#	x: 
	#
	# Returns: data.frame
	x.sort <- sort(table(x),TRUE)	# sorted table for the vector
	x <- data.frame(count=x.sort, total=cumsum(x.sort), percent=round(100*x.sort/sum(x.sort),2), cum_percent=round(100*cumsum(x.sort)/sum(x.sort),2)) # clean up table
	return(x)
}