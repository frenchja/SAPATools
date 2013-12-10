PairwiseDescribe <- function(x) {
	# Author:  David M. Condon and Jason A. French
	# Args:
	#	  x:	SAPA data.frame
	# Returns:  mean and sd of pairwise administrations
	countXItems <- count.pairwise(x)
	lowleft <- lower.tri(countXItems, diag = FALSE)
	countXItems[!lowleft] <- NA
	mean <- mean(countXItems, na.rm=TRUE)
	median <- median(countXItems, na.rm=TRUE)
	sd <- sd(countXItems, na.rm=TRUE)
	min <- min(countXItems, na.rm=TRUE)
	max <- max(countXItems, na.rm=TRUE)
	results <- list(mean=mean, median=median, sd=sd, min=min, max=max)
	return(results)
}