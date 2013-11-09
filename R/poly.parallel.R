polychoric <- function (x, smooth = TRUE, global = TRUE, polycor = FALSE, ML = FALSE, 
   std.err = FALSE, progress = TRUE, parallel = FALSE, n.cpu = 1) 
{
   if (!require(mvtnorm)) {
       stop("I am sorry, you must have mvtnorm installed to use polychoric")
   }
   if (polycor && (!require(polycor))) {
       warning("I am sorry, you must have  polycor installed to use polychoric with the polycor option")
       polycor <- FALSE
   }
   if (parallel) {
       if (!require(doMC))  {
        warning('parallel not used.  Please install doMC!')
        parallel <- FALSE
       }
      if (!require(foreach))  {
        warning('parallel not used.  Please install foreach!')
        parallel <- FALSE
      }
      if (!require(multicore))  {
        warning('parallel not used.  Please install multicore!')
        parallel <- FALSE
       }
       if(!n.cpu) {
        n.cpu <- multicore:::detectCores()
       }
       registerDoMC(cores=n.cpu)
   }
   cl <- match.call()
   nvar <- dim(x)[2]
   nsub <- dim(x)[1]
   x <- as.matrix(x)
   xt <- table(x)
   nvalues <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE) + 
       1
   if (nvalues > 8) 
       stop("You have more than 8 categories for your items, polychoric is probably not needed")
   xmin <- apply(x, 2, function(x) min(x, na.rm = TRUE))
   x <- t(t(x) - xmin + 1)
   xfreq <- apply(x, 2, tabulate, nbins = nvalues)
   n.obs <- colSums(xfreq)
   xfreq <- t(t(xfreq)/n.obs)
   tau <- qnorm(apply(xfreq, 2, cumsum))[1:(nvalues - 1), ]
   if (!is.matrix(tau)) 
       tau <- matrix(tau, ncol = nvar)
   rownames(tau) <- names(xt)[1:(nvalues - 1)]
   colnames(tau) <- colnames(x)
   mat <- matrix(0, nvar, nvar)
   colnames(mat) <- rownames(mat) <- colnames(x)
   x <- x - min(x, na.rm = TRUE) + 1
   foreach(i=2:nvar) %dopar% {
   #for (i in 2:nvar) {
       if (progress) 
           progressBar(i^2/2, nvar^2/2, "Polychoric")

       	for (j in 1:(i - 1)) {
       	    if (t(!is.na(x[, i])) %*% (!is.na(x[, j])) > 2) {
       	        if (!polycor) {
       	          poly <- polyc(x[, i], x[, j], tau[, i], tau[, 
       	            j], global = global)
       	          mat[i, j] <- mat[j, i] <- poly$rho
       	        }
       	        else {
       	          poly <- polychor(x[, i], x[, j], ML = ML, std.err = std.err)
       	          mat[i, j] <- mat[j, i] <- poly
       	        }
       	    }
       	    else {
       	        mat[i, j] <- mat[j, i] <- NA
       	    }
       	}
       }
   diag(mat) <- 1
   if (any(is.na(mat))) {
       warning("some correlations are missing, smoothing turned off")
       smooth <- FALSE
   }
   if (smooth) {
       mat <- cor.smooth(mat)
   }
   tau <- t(tau)
   result <- list(rho = mat, tau = tau, n.obs = nsub, Call = cl)
   if (progress) {
       cat("\n")
       flush(stdout())
   }
   class(result) <- c("psych", "poly")
   return(result)
}