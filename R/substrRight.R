substrRight <- function(x, n){
    # Trims down values in vector x to n char
    # 
	sapply(x, function(xx)
		substr(xx, (nchar(xx)-n+1), nchar(xx))
	)
}
