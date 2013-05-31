mixed.cor <-
function (x = NULL, p = NULL, d = NULL, smooth = TRUE, correct = TRUE, 
          global = TRUE, ncat = 8, polycor = FALSE) 
{
  cl <- match.call()
  organize <- FALSE
  if (!is.null(x) && is.null(p) && is.null(d)) {
    organize <- TRUE
    nvar <- ncol(x)
    x <- as.matrix(x)
    tab <- apply(x, 2, function(x) table(x))
    if (is.list(tab)) {
      len <- lapply(tab, function(x) length(x))
    }
    else {
      len <- dim(tab)[1]
    }
    dvars <- subset(1:nvar, len == 2)
    pvars <- subset(1:nvar, ((len > 2) & (len <= ncat)))
    cvars <- subset(1:nvar, (len > ncat))
    if (length(dvars) > 0) {
      d <- matrix(x[, dvars], ncol = length(dvars))
      colnames(d) <- colnames(x)[dvars]
    }
    else {
      d <- NULL
    }
    if (length(pvars) > 0) {
      p <- matrix(x[, pvars], ncol = length(pvars))
      colnames(p) <- colnames(x)[pvars]
    }
    else {
      p <- NULL
    }
    if (length(cvars) > 0) {
      cont <- matrix(x[, cvars], ncol = length(cvars))
      colnames(cont) <- colnames(x)[cvars]
    }
    else {
      cont <- NULL
    }
    Rho <- mixed.cor1(cont, p, d, smooth = smooth, polycor = polycor, 
                      global = global, correct = correct)
    oldorder <- c(cvars, pvars, dvars)
    ord <- order(oldorder)
    Rho$rho <- Rho$rho[ord, ord]
  }
  else {
    Rho <- mixed.cor1(x = x, p = p, d = d, smooth = smooth, 
                      polycor = polycor, global = global, correct = correct)
  }
  Rho$Call <- cl
  return(Rho)
}
<environment: namespace:psych>