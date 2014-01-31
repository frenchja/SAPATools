PairwiseDescribe <- function(x) {
	# Author:  David M. Condon and Jason A. French
	# Args:
	#	  x:	SAPA data.frame
	# Returns:  descriptives for pairwise administrations
	cp <- count.pairwise(x,diagonal=FALSE)
	cp <- as.vector(cp[lower.tri(cp)])
	describe(cp,skew=FALSE)
}
