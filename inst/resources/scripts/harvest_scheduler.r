


## this is a simple simulate annealing harvest scheduler in R


## load the data...
yields <- read.table( "../data/leuschner.txt", header=FALSE )
names( yields ) <- c("stand","period","age","volume")

# demands <- read.table( "../data/demands.txt", header=TRUE, sep="," ) 
# pdx <- read.table( "../data/pdx.txt", header=TRUE, sep="," ) 


unit <- 1:50
period <- as.integer( runif( 50 ) * 10 ) + 1
age.now <- as.integer( runif( 50 ) * 100 ) + 1
hs <- as.data.frame( cbind( unit, age.now, period ) )

## for now, create some fake targets and productions
n.per <- 20
targets <- runif( n.per ) * 15
t2 <- rep( mean( targets), n.per )
targets <- t2
pdx <- runif( n.per ) * 15

# x <- xyplot( volume ~ period | stand, data=yields, type="l" )
# print( x )

## now create an objective function to optimize the harvest
## subject to some constraint
obj.func <- function( sq ) {


  ## sq is the harvest period for stand i
  
  
  crossprod( pdx[sq] - targets )


  ## this is the function where you need to compute the harvest volume
  ## but also the volume for the replanted stands
  

}


genseq1 <- function(sq) {   

##  idx <- seq(2, NROW(targets), by=1)
  idx <- seq(1, NROW(targets), by=1)

  ## sample to find to cells to swap
  changepoints <- sample(idx, size=2, replace=FALSE)

  tmp <- sq[changepoints[1]]
  sq[changepoints[1]] <- sq[changepoints[2]]
  sq[changepoints[2]] <- tmp
  return(sq)
}

# print( pdx )
# print( demands )


sq1 <- 1:n.per
o <- obj.func( genseq1( sq1 ) )

set.seed(2222) # chosen to get a good soln quickly
res <- optim(sq1,
             obj.func,
             genseq1,
             method="SANN",
             control = list(maxit=30000,
               temp=200,
               trace=TRUE) )


print( res )



## plot the results
plot( targets, type="l", ylim=c(0,20) )
par( new=TRUE )
plot( pdx[res$par], type="l", ylim=c(0,20), col="red" )


## par( new=TRUE )
## plot( pdx$act.saw[res$par], type="l", ylim=c(50,200), col="blue" )

## now plot the predicted production vs. demands
## plot( demands$saw - pdx$act.saw[res$par], type="l", ylim=c(-100,100) )
## abline( h=0 )


## you'll want to compute the npv in your objective function as well...
## create a vector of the times
year <- 0:10*10
i <- 0.04
## and discount $100 at 4%  
100.0*( 1 - i )^year



