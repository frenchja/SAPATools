generate.scatter <- function(cor, n.obs = 500, add.text = FALSE, save = FALSE) {
  # Author: Jason A. French
  # Args:
  #   cor = List of desired correlations
  #   n.obs = Number of observations
  #   add.text = Annotate correlation to graph
  #   save = Save graph as PNG to current directory
  n.obs <- n.obs
  
  # Generate random correlation if not specified
  if(missing(cor)){
    cor <- runif(n = 1, min = -1, max = 1)
  }
  lapply(X = cor,FUN = function(cor, ...){
    # Generate matrix code modified from r-bloggers.com
    R = matrix(cbind(1,cor,cor,1),nrow=2)
    U = t(chol(R))
    nvars = dim(U)[1]
    random.normal = matrix(rnorm(nvars*n.obs,0,1), nrow=nvars, ncol=n.obs);
    X = U %*% random.normal
    newX = t(X)
    raw = as.data.frame(newX)
    library(ggplot2)
    p <- ggplot(raw, aes(x = V1, y = V2)) + geom_point()
    if(isTRUE(add.text)){
      cor.text <- paste('R = ',as.character(round(cor(raw)[2],2)),sep='')
      p <- p + annotate("text", x = 2, y = -1, label = cor.text,size=10)
    }
    print(cor(raw)[2])
    if(isTRUE(save)){
      ggsave(filename = paste(cor,'.png',sep=''),plot = p)
    } else {
      return(p)
    }
  }) 
}