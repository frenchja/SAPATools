"sapa.zip" <-
   function(data) {
    valid <- function(x) { #count the number of valid cases 
        sum(!is.na(x))
    }
    
   if(!require(zipcode)) {stop("The package zipcode must be installed:")}
   data(zipcode)
   zips <- zipcode
   zips$zip <- as.numeric(zips$zip)  #because zipcodes are character
       z <-  data[,"zip"]
       cnames <- colnames(data)
       for (i in 1:ncol(data)) {if(is.factor(data[,i]) || is.logical(data[,i])) {
             data[,i] <- as.numeric(data[,i]) }}
       xvals <- list()
               xvals$mean <- t(matrix(unlist(by(data,z,colMeans,na.rm=TRUE)),nrow=ncol(data)))              
               xvals$n <- t(matrix(unlist(by(data,z,function(x) sapply(x,valid))),nrow=ncol(data)))
    colnames(xvals$mean) <-  colnames(data)
    colnames(xvals$n) <- paste(colnames(xvals$mean),".n",sep="")
    rownames(xvals$mean) <-  rownames(xvals$n) <- xvals$mean[,"zip"]

   sapa.values  <- cbind(xvals$mean,xvals$n)
   sapa.zip <- merge(sapa.values,zips,by="zip")
   return(sapa.zip)}
   
"sapa.clean" <- 
function(data,database="usa",DV="g")  {
 map<- map(database,plot=FALSE)
 rangex <- map$range[2] - map$range[1]
 rangey <-  map$range[4] - map$range[3]
 
# data["longitude"] <-  round((data["longitude"] - map$range[1])*gridsize/rangex)
# data["latitude"] <-  round((data["latitude"] - map$range[3])*gridsize/rangey)
 clean <- subset(data,(data[,"longitude"] > map$range[1]) )
 clean <- subset(clean,(clean[,"longitude"] < map$range[2]))
 clean <- subset(clean,(clean[,"latitude"] > map$range[3]))
 clean <- subset(clean,(clean[,"latitude"] < map$range[4]))
 result <- data.frame(longitude= clean[,"longitude"], latitude = clean[,"latitude"],DV=clean[,DV])
return(result)
}    
   
"sapa.grid" <- 
function(df,DV,gridsize=300,database="state",average=TRUE,regions=".") {
 map<- map(database=database,plot=FALSE,regions=regions)
 if(average) {aver = mean(df[,DV],na.rm=TRUE)} else {aver=0}
 df[,DV] <- df[,DV] -aver   #center the data around the average
 mat <- matrix(0,gridsize,gridsize)
 count <- matrix(0,gridsize,gridsize)
 nsubs <- nrow(df)
 rangex <- map$range[2] - map$range[1]
 rangey <-  map$range[4] - map$range[3]
 df["longitude"] <-  round((df["longitude"] - map$range[1])*gridsize/rangex)
 df["latitude"] <-  round((df["latitude"] - map$range[3])*gridsize/rangey)
 clean <- subset(df,(df[,"longitude"] > 0) )
 clean <- subset(clean,(clean[,"longitude"] < (gridsize + 1)))
 clean <- subset(clean,(clean[,"latitude"] < (gridsize + 1)))
 clean <- subset(clean,(clean[,"latitude"] >  0))
 nsubs <- nrow(clean)
 DVn <- paste(DV,".n",sep="")
 for (observation in (1:nsubs)) {
    long <- clean[observation,"longitude"]
    lat <- clean[observation,"latitude"]
    if(!is.na(clean[observation,DV])) {
     mat[long,lat] <- mat[long,lat] + clean[observation,DV]*clean[observation,DVn] 
    
     count[long,lat] <- count[long,lat] + clean[observation,DVn]  } 
}
av <- mat/count
if(average) {av[!is.finite(av)] <- 0} else {av[!is.finite(av)] <- aver}
return(list(average=av,total=mat,count=count))
}

"sapa.smooth" <- 
function(grid,size=11,miss=.05,w=NULL) {

n <- grid$count
n[n==0] <- miss
tot <- grid$tot
nvar <- ncol(tot)
if (is.null(w) ) {

w <- matrix(1,size,size)
center <- median(1:size)
for (i in 1:size) {
for (j in 1:size) {
  w[i,j] <- (sqrt((i-center)^2 + (j-center)^2))/2
  w[i,j] <- dnorm(w[i,j])
  }
  }}
pad <- center-1
padding <- matrix(0,nvar,pad)
tot1 <- cbind(padding,tot,padding)
n1 <- cbind(padding,n,padding)
padding <- matrix(0,pad,ncol(tot1))
tot1 <- rbind(padding,tot1,padding)
n1 <- rbind(padding,n1,padding)

#now smooth every cell of grid using grid1 as the source
for (i in (pad+1):(nvar+pad)) {
  for (j in (pad+1):(nvar+pad)) {
       tot[(i-pad),(j-pad)] <- sum(tot1[(i-pad):(i+pad),(j-pad):(j+pad)]  *w)/ sum(n1[(i-pad):(i+pad),(j-pad):(j+pad)]  *w) 
 } }
return(tot)
  }
  
 
 "sapa.inside" <- 
   function(grid,database="usa",regions=".") {
    if(!require(maps)) {stop("The package maps must be installed:")}
   if(!require(mgcv)) {stop("The package mgcv must be installed")}
   nlat = nrow(grid)
   nlong =ncol(grid)
   us <- map(database=database,plot=FALSE,regions=regions)
   bnd <- data.frame(x=us$x,y=us$y)
   sapa <- matrix(0,nlat*nlong,3)
   x <- (rep(1:nlat,each=nlong)/nlong) *(us$range[2]-us$range[1]) + us$range[1]
   y <- (rep(1:nlong,nlat)/nlat)  *(us$range[4]-us$range[3]) + us$range[3]
   z <- as.vector(t(grid))
   #colnames(sapa) <- c("x","y","z")
   is.in <- inSide(bnd,x,y)
   x <- x[is.in]
   y <- y[is.in]
   z <- z[is.in]
   n.in <- length(x)
   zgrid <- matrix(NA,nlat,nlong)
   for (location in 1:n.in) {
      lat <- round((y[location]-us$range[3])*nlat/(us$range[4]-us$range[3]))
      long <- round((x[location]- us$range[1])*nlong/(us$range[2]-us$range[1]))
      zgrid[long,lat] <- z[location] }
  
   newgrid <- list(x= (1:nlong)*(us$range[2]-us$range[1])/nlong + us$range[1],
                y = (1:nlat) *(us$range[4]-us$range[3])/nlat + us$range[3],
                z = zgrid)
                
   return(list(x=x,y=y,z=z,grid=newgrid))
   }
  
  

"sapa.image" <- 
function(grid,ncols=NULL,main="SAPA map",database="state",regions=".") {
if(is.list(grid)) grid <- grid$grid
 map <- map(database=database,plot=FALSE)
 rangex <- map$range[2] - map$range[1]
 rangey <-  map$range[4] - map$range[3]
if(is.null(ncols)) ncols <- ncol(grid$z)
 gr <- colorRampPalette(c("red","white","blue")) 
    colramp  <- gr(ncols)
image(grid,col=colramp,axes=FALSE,main=main)
map <- map(database,add=TRUE,regions=regions)
}

"sapa.plot" <- 
function(xyz,main="SAPA map",database="state",n=21,range=c(-3,3))
{ 
map(database,plot=FALSE)
 gr <- colorRampPalette(c("red","white","blue")) 
    colramp  <- gr(n)
x <- xyz$x
y <- xyz$y
z <- xyz$z
z [z< range[1]] <- range[1]
z [z > range[2]] <- range[2] 
minz <- min(z,na.rm=TRUE)
maxz <- max(z,na.rm=TRUE)
bottom <- min(minz,-maxz)
top <- max(-minz,maxz)
z1 <- (z-bottom)*n/(top-bottom)
points(x,y,col=colramp[z1],pch=19)  
map(database,add=TRUE)
title(main=main)
}


"sapa.combined" <- 
function(df,DV,gridsize=300,database="usa",regions=".",average=TRUE,size=11,miss=.05,ncols=NULL,main="SAPA combined") {
   grid <- sapa.grid(df=df,DV=DV,gridsize=gridsize,database=database,average=average,regions=regions)
   grid <- sapa.smooth(grid,size=size,miss=miss)
   grid <- sapa.inside(grid,database=database,region=regions)
   sapa.image(grid,ncols=ncols,main=main,database="state",regions=regions)
   invisible(grid)
   }
