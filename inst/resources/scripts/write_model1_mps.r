## this script generates a model 1 mps file for the rufus dataset
## you need to add the spatial constraints using the adj_list stuff....
## this r script generates the mps file for a model 1 harvest schedule for rufus
## R --slave --vanilla < R/write_model1_mps.r $1
## where $1 is the rufus schema (client)
## this chunk is what you're going to add to the springer book
## you're going to use this file for a ton of southern journal papers.

## make sure you start with a clean slate!!!
rm( list=ls() )

## this function returns a list object that contains
## a set of N variables, where each variable can be
## a scalar, vector, or matrix of data you want to
## keep track of by polygon

## that the entry for the
## p == n periods in planning horizon (using for bincombinations)
## t == veg.type (used for table lookup)
## e == existing yields (table of existing forest conditions)
## r == regeneration yields (table of regenerated forest conditions)
## min.delay == number of periods between final harvests (non-spatial green up time)
## acres == area of the polygon
print( "defining polygon.rx.rows()" )
polygon.rx.rows <- function( p,
                            veg.type,
                            e, ## are these supposed to be matricies!?
                            r, ## are these supposed to be matricies!?
                            min.delay,
                            acres ) {

  mask <- bincombinations( p )
  ret.val <- vector( "list", nrow(mask))
  
  for( i in 1:nrow(mask) ) {

    ## extract the periods that contain harvests
    cut.periods <- (1:p)[mask[i,] == 1]
    n.cuts <- length( cut.periods )
    
    ## the first harvest is from the existing yields
    y.e <- rep( 0, length( mask[i,] ) )
    y.r <- rep( 0, length( mask[i,] ) )
        
    if( n.cuts == 0 ) {
      is.valid <- TRUE
    }

    ## if there's only one harvest, then yields are
    ## from the existing yields table
    if( n.cuts == 1 ) {
      y.e[cut.periods[1]] <- e[cut.periods[1],veg.type]
      is.valid <- TRUE
    }

    ## if there's more than a single harvest
    if( n.cuts > 1 ) {
      y.e[cut.periods[1]] <- e[cut.periods[1],veg.type]
      diff.periods <- diff( cut.periods )
      y.r[cut.periods[2:length(cut.periods)]] <- r[diff.periods,veg.type]
      
      ## determine if there's enough delay between harvests
      ok.delays <- sum( diff.periods >= min.delay )
      
      if( ok.delays == length( diff.periods ) ) {
        is.valid <- TRUE
      } else {
        is.valid <- FALSE
      }
    }

    ## build the return structure (one for each rx in the polygon)
    ret.val[[i]]$mask <- mask[i,]
    ret.val[[i]]$n.cuts <- n.cuts
    ret.val[[i]]$cut.periods <- cut.periods ## these represent the columns that need to be gen'd
    ##ret.val[[i]]$cut.adj <- adj.deg * mask[i,]
    ret.val[[i]]$cut.e <- cut.periods[1]

    if( n.cuts > 1 ) {
      ## these are regeneration cuts
      ret.val[[i]]$cut.r <- cut.periods[2:length(cut.periods)]
    } else {
      ## there are no regeneration harvests
      ret.val[[i]]$cut.r <- NA
    }  

    ret.val[[i]]$is.valid <- is.valid
    ret.val[[i]]$vol.e <- y.e * acres
    ret.val[[i]]$vol.r <-  y.r * acres
    ret.val[[i]]$vol <- ( y.e + y.r ) * acres
    ret.val[[i]]$act.acres <- mask[i,] * acres

    ## calculate the npv of the resulting volumes
    ## this depends on the type of stand you start with
    ## assumptions from page 
    ## you should make a table out of this...
    if( veg.type == 1 ) {
      ## healthy "old-growth"
      ret.val[[i]]$gross.sales.e <- y.e * 100 * 4 * acres
      ret.val[[i]]$gross.sales.r <- y.r * 100 * 2.5 * acres
      ret.val[[i]]$logging.costs.e <- y.e * 100 * 1 * acres
      ret.val[[i]]$logging.costs.r <- y.r * 100 * 0.75 * acres
      ret.val[[i]]$site.prep <- mask[i,] * 500 * acres
      ret.val[[i]]$admin <- rep( 1, p ) * 3 * 10 * acres
    }

    if( veg.type == 2 ) {
      ## "diseased old growth"
      ret.val[[i]]$gross.sales.e <- y.e * 100 * 2 * acres
      ret.val[[i]]$gross.sales.r <- y.r * 100 * 2.5 * acres
      ret.val[[i]]$logging.costs.e <- y.e * 100 * 1.50 * acres
      ret.val[[i]]$logging.costs.r <- y.r * 100 * 1.25 * acres
      ret.val[[i]]$site.prep <- mask[i,] * 300 * acres
      ret.val[[i]]$admin <- rep( 1, p ) * 2 * 10 * acres
    }

    if( veg.type == 3 ) {
      ## "young growth and regen" poor site
      ret.val[[i]]$gross.sales.e <- y.e * 100 * 2.5 * acres
      ret.val[[i]]$gross.sales.r <- y.r * 100 * 2.5 * acres
      ret.val[[i]]$logging.costs.e <- y.e * 100 * 1.25 * acres
      ret.val[[i]]$logging.costs.r <- y.r * 100 * 1.25 * acres
      ret.val[[i]]$site.prep <- mask[i,] * 300 * acres
      ret.val[[i]]$admin <- rep( 1, p ) * 2 * 10 * acres
    }
    
    ## assume all costs are applied in the middle of the period
    ret.val[[i]]$total.costs <- ret.val[[i]]$logging.costs.e  + 
      ret.val[[i]]$logging.costs.r +
        ret.val[[i]]$site.prep + 
          ret.val[[i]]$admin 
    
    ret.val[[i]]$net.rev <- ret.val[[i]]$gross.sales.e +
      ret.val[[i]]$gross.sales.r -
        ret.val[[i]]$total.costs
    
    yrs <- (1:p + 0.5) * 10 - 10

    ## this should be an argument
    ret.val[[i]]$disc <- ( 1 + 0.04 ) ^ (-yrs)

    ret.val[[i]]$npv <- ret.val[[i]]$net.rev * ret.val[[i]]$disc
    ret.val[[i]]$sum.npv <- sum( ret.val[[i]]$npv )

    ## for rx[i], what is the column values for the adj rows?
    ##ret.val[[i]]$adj.deg <- rep( adj.deg, p )
  }
    
  ret.val

}


## extract the data and masks (using sapply)
## this function returns a set of columns of the variables of interest
print( "defining get.dcm()" )
get.dcm <- function( x ) {

  ## unroll the adj matrix out...
  ret.val <- cbind( x$vol.e,
                   x$vol.r,
                   x$vol, 
                   x$total.cost,
                   x$net.rev,
                   x$npv, 
                   x$act.acres,
                   x$mask )
  ret.val
}

## this function returns a dcm for the stand and prescription set
## which is an N x M variables matrix
## the format of the output is a set of rows and columns that 
## are used to generate the mps file
print( "defining stand.dcm()" )
stand.dcm <- function( id, n.periods, veg.type, ylds.m, regen.ylds.m, delay, acres ) {
  
  rx.rows <- polygon.rx.rows( per, veg.type, ylds.m, regen.ylds.m, delay, acres )
  ##print( "display the rx.rows matrix - complete" )
  ##print( rx.rows )
  
  rx.col <- sapply( rx.rows, get.dcm )
  
  ## add the npvs for the stand,rx
  is.valid <- sapply( rx.rows, function(x) x$is.valid )

  ## add the npvs for the stand,rx
  npvs <- sapply( rx.rows, function(x) x$sum.npv )
  rx.col <- rbind( rx.col, npvs )
  
  ## per.adj.rows <- sapply( rx.rows, get.per.adj.rows )
  ## get the vector for the adjacency constraints
  ##rx.col <- rbind( rx.col, adj.cols )
  
  ## definitions
  ## hvole == harvest volume of existing stands in period i
  ## hvolr == harvest volume of regen stands in period i
  ## hvolt == harvest volume (total) in period i
  ## tc == total operating costs in period i
  ## nr == net revenue in period i
  ## npvc == net present value component TODO: define this...
  ## hac == habitat acres? TODO: define
  ## cut == is.cut in period i (used for tracking the number of units to manage in period i -- admin)
  rowlabs <- c(paste( "hvole", 1:n.periods, sep=""), 
               paste( "hvolr", 1:n.periods, sep=""), 
               paste( "hvolt", 1:n.periods, sep=""), 
               paste( "tc", 1:n.periods, sep=""), 
               paste( "nr", 1:n.periods, sep=""), 
               paste( "npvc", 1:n.periods, sep=""), 
               paste( "hac", 1:n.periods, sep=""),
               paste( "cut", 1:n.periods, sep=""),
               "npv"
               )

  ## assign the rownames
  rownames( rx.col ) <- rowlabs
  
  ## the column names define the decision variables
  colnames( rx.col ) <- paste( "s", id, "rx", 1:(ncol(rx.col)), sep="" )
  
  ## only return "valid" prescriptions
  ## you need another if() switch here... (another paper for sjaf?)
  valid.rxs <- rx.col[,is.valid]
  ##  print( valid.rxs )
  
  valid.rxs
}


################################################################################
################################################################################
################################################################################
################################################################################

print( commandArgs() )
debug <- TRUE

library( e1071, verbose=debug ) ## contains bincombinations
library( glpk ) ## read the mps file using the rglpk routines
library( RODBC, verbose=debug ) ## contains the odbc i/o functions

##dsn <- commandArgs()[4]
#mps.filename <- paste( "tmp/", dsn, ".mps", sep="" ) 
# use.adj == include adjacnecy constraints? (0=no,1=yes) (see brodie and yashimoto xxx)
# use.evenflow == include even flow constraints on volume

dsn <- "danpickett"
mps.filename <- paste( dsn, ".mps", sep="" ) 


################################################################################
## connect to the rufus database for the client dsn
channel <- odbcConnect( dsn="rufus", uid="postgres" )

################################################################################
## get the config data for this ownership, stands, yields, etc..
per <-  7
min.delay <- 0
include.adj <- 0
include.evenflow <- 0

print( "load data.frames..." )
sql.str <- sprintf( "select id,strata,area(boundary)/43560.0 as acres from %s.stands order by id;", dsn ) 
stands <- sqlQuery(channel, sql.str )

sql.str <- sprintf( "select * from %s.adj_list order by id;", dsn ) 
adj <- sqlQuery(channel, sql.str )

sql.str <- sprintf( "select * from %s.rx;", dsn ) 
rxs <- sqlQuery(channel, sql.str )

print( "replace test data.frames..." )
per <-  2
n.stands <- 4
stands <- data.frame( id=c(1,2,3,4), strata=c(1,2,3,1), acres=rep( 1, n.stands ) )
adj <- data.frame( id=c(1,1,2,2,3,3,4,4), adj_id=c(2,3,1,4,1,4,2,3) )
rxs <- bincombinations( 2 )

################################################################################
## close the odbc channel
if( channel != -1 ) {
   odbcClose( channel )
}

################################################################################
## this is going to go back into the simulation chapter.
## chapter 9 springer book

## you need to import these into rufus
## present yields, move these into rufus
## where in d&j did these come from
## They are from table 13.16 in davis and johnson (200X)
period <- c(1,2,3,4,5,6,7,8,9,10)
a <- c(35,37,38.5,40,41.5,42.5,43.5,44,44.5,45)
b <- c(18,17.5,16.5,15,14,13,12.5,12,11.5,11)
c <- c(6,11,15,17.9,19.5,21.5,23,24.5,26,27)

## future yields for 10 periods into the future.
a.regen <- c(3,16,26,30.5,33.5,35.5,37.5,39.0,40.0,41)
b.regen <- c(0,5,12,16,18.5,20.5,22.5,24,25.5,27 )
c.regen <- c(0,5,12,16,18.5,20.5,22.5,24,25.5,27 )

## THESE ARE SUPPOSED TO BE MATRICIES!!!
ylds.m <- as.matrix( cbind( a, b, c ) )
regen.ylds.m <- as.matrix( cbind( a.regen, b.regen, c.regen ) )

################################################################################
## print out some stuff for debugging
if( debug ) {
  print( mps.filename )
  print( stands )
  print( ylds.m )
  print( regen.ylds.m )
}


################################################################################
################################################################################
################################################################################
## these need to wrapped up into functions that generate the four data frames
## you need to write out the mps file...
## then convert the mps file into the other format using the glpk api
################################################################################
################################################################################
################################################################################


################################################################################
## you will have at least four data.frames here (rows+rhs,cols+bounds)
rows.frame <- NULL
rhs.frame <- NULL
cols.frame <- NULL
bounds.frame <- NULL

################################################################################
## build the ROWS data.frame ( row = name of variable, type)

## for now, just use NPV to compare with davis and johnson
## the first one is the objective
## select the objective function (model 1 only gets one objective)
## max/min direction is controlled from the command line
rows.frame <- data.frame( row="npv", dir="N" )

## these are accounting variables and are tracked for EVERYTHING!
## landscape level values by period
rowlabs <- c(paste( "hvole", 1:per, sep=""), 
             paste( "hvolr", 1:per, sep=""), 
             paste( "hvolt", 1:per, sep=""), 
             paste( "tc", 1:per, sep=""), 
             paste( "nr", 1:per, sep=""), 
             paste( "npvc", 1:per, sep=""), 
             paste( "hac", 1:per, sep=""),
             paste( "cut", 1:per, sep="") )
rows.frame <- rbind( rows.frame, data.frame( row=rowlabs, dir="N" ) )

## include the set of rows that define the constraint to assign
## a single rx (i.e. sum x_{i} = 1) 
rows.frame.1rx <- data.frame( row=paste( "s", 1:nrow(stands), "rx", sep="" ), dir="E" )
rows.frame <- rbind( rows.frame, rows.frame.1rx )


################################################################################
################################################################################
## build the COLS and bounds for each stand
## while you're doing that, begin to construct the bounds as well.
## loop over the stands to generate the columns
for( i in 1:nrow(stands) ) {

  ## these are the fields you need for this section
  stand.id <- stands[i,]$id
  veg.type <- as.numeric( stands[i,]$strata )
  stand.acres <- stands[i,]$acres
  print( paste( "building primary DCM for stand", i, stand.id, veg.type, stand.acres ) )
    
  ## get the dcm for the stand
  ## sdcm is a matrix that contains the data for all rx for a single stand
  ##sdcm <- stand.dcm( stand.id, per, veg.type, ylds.m, regen.ylds.m, 3, stand.acres )
  sdcm <- stand.dcm( stand.id, per, veg.type, ylds.m, regen.ylds.m, min.delay, stand.acres )

  ##print( sdcm )  
  ## this get's the 19th, 13th, 9th, 6th, and 1st VALID RX (not the 19,13,9,6,1 from bincombinations)
  ##sdcm <- sdcm[,c(19,13,9,6,1)]
  ## now, you have a matrix that contains M stand variables (rows) X N rx's (columns)
  
  ## for each prescription in the stand detached coefficient matrix
  ## generate the column associated with the prescription
  for( j in 1:ncol( sdcm ) ) {
    ## rcc.frame is the "mps" file frame that will be stored in rufus
    rcc.frame <- data.frame(row   = names( sdcm[,j]),
                            col  = colnames(sdcm)[j], 
                            coeff = sdcm[,j] )
    
    cols.frame <- rbind( cols.frame, rcc.frame )
  }

  ## create a row for each stand so that 
  ## one row gets attached for all the columns generated
  cols.frame.1rx <- data.frame(
                               row=paste( "s", stand.id, "rx", sep="" ),
                               col=colnames( sdcm ),
                               coeff=1
                               )
  cols.frame <- rbind( cols.frame, cols.frame.1rx )
    
  ## set the columns (decision variables) to binary (0/1) for this stand and add it to the main bounds.frame
  ## for each column (rx) in each stand, write
  ## out the row to set the decision variables to binary
  bounds.frame <- rbind( bounds.frame, data.frame( col=colnames( sdcm ), type="BV", lab="BND1" ) )
 
}


################################################################################
## build the RHS data.frame
## add the rows to set rx for each stand to only one rx
rhs.frame.1rx <- data.frame( lab=rep( "RHS1", nrow( stands ) ),
                            row=paste( "s", 1:nrow(stands), "rx", sep="" ),
                            bound=1 )
rhs.frame <- rbind( rhs.frame, rhs.frame.1rx )


################################################################################
## if you want to include adjacency constraints
if( include.adj ) {
  print( "building DCM entries for adjacency constraints... begin" )
  
  ## build the adj matrix  
  ## build the RHS for the adj constraint (good - do not modify)
  print( "creating the adjacency matrix entries..." )
  adj.m <- matrix( 0, max( adj$id ), max( adj$id ) )
  for( i in 1:nrow(adj) ) {
    adj.m[adj[i,]$id,adj[i,]$adj_id] <- 1
  }
  
  per.adj.rowlabs <- NULL
  for( i in 1:nrow( stands )) {
    per.adj.rowlabs <- c( per.adj.rowlabs, paste( "s", i, "p", 1:per, sep="" ) )
  }
  
  print( "creating RHS for period adjacency" )
  adj.deg <- rowSums( adj.m )  ## do you really need this?
  per.adj.rhs.frame <- data.frame( lab=rep( "RHS1", nrow( stands ) ),
                                  row=per.adj.rowlabs,
                                  bound=kronecker( adj.deg, rep( 1, per ) )
                                  )
  
  ## build the ROWS frame for the adj constraint
  ## you only need to do this once over all stands and periods
  ## becuase you already have the rows defined
  ## (good) do not modify
  print( "creating ROWS for period adjacency" )
  per.adj.rows.frame <- data.frame( row=per.adj.rowlabs, dir="L" )
  
  ## build the coeffs.frame for the adj constraints...
  ## (good) do not modify
  print( "creating COLUMNS for period adjacency" )
  n.stands <- nrow( stands )
  ad <- adj.m + diag( adj.deg )
  per.adj.coeffs.frame <- kronecker( ad, t( bincombinations( per ) ) )
  rownames( per.adj.coeffs.frame ) <- per.adj.rowlabs
  
  colnames( per.adj.coeffs.frame ) <- paste( "s", kronecker( 1:n.stands, rep( 1, 2^per ) ), "rx", 1:(2^per), sep="" )
  
  per.adj.cols.frame <- NULL
  for( j in 1:ncol( per.adj.coeffs.frame ) ) {  
    rcc.frame <- data.frame(row   = names( per.adj.coeffs.frame[,j] ),
                            col  = colnames(per.adj.coeffs.frame)[j], 
                            coeff = per.adj.coeffs.frame[,j] )

    ## you might want to think about writting this out to a stream
    ## and reading in later...
    per.adj.cols.frame <- rbind( per.adj.cols.frame, rcc.frame )

    ## you're right here...
    ##print( j )
  }
  
  ## you don't need to build the bounds since there are no additional columns
  print( "building DCM entries for adjacency constraints... complete" )

  print( "appending the DCM entries for adjacency constraints..." )
  rhs.frame <- rbind( rhs.frame, per.adj.rhs.frame )
  rows.frame <- rbind( rows.frame, per.adj.rows.frame )
  cols.frame <- rbind( cols.frame, per.adj.cols.frame )
  
}


################################################################################
## if you want to include even flow constraints
if( include.evenflow ) {
  print( "building DCM entries for even flow constraints... begin" )
  print( "building DCM entries for even flow constraints... complete" )  
  ##   rhs.frame <- rbind( rhs.frame, rhs.frame.evenflow )
}

  

################################################################################
################################################################################
## write out the freemps file.
##mps.filename <- paste( dsn, ".0.0.mps", sep="" )
mps.filename <- sprintf( "%s.%d.%d.mps", dsn, include.adj, include.evenflow )
mps.file <- file( mps.filename, "w")  # open an output file connection
##cat( "NAME danpickett_adj_0_evenflow_0", file = mps.file, sep = "\n")
cat( sprintf( "NAME %s", mps.filename ), file = mps.file, sep = "\n")

## write the rows data.frame to the file
print( "writing ROWS..." )
cat("ROWS", file = mps.file, sep = "\n")
cat( sprintf( " %1s %s", rows.frame$dir, rows.frame$row ), file = mps.file, sep = "\n")

## write the cols data.frame to the file
print( "writing COLUMNS..." )
cat("COLUMNS", file = mps.file, sep = "\n")

## rbind the cols.frames
cf <- cols.frame[order( cols.frame$col ),]
cf <- cf[cf$coeff != 0,]
cf2 <- cf[order( cf$col ),]
cat( sprintf( " %s %s %f", cf2$col, cf2$row, cf2$coeff ), file = mps.file, sep = "\n")

## write the right-hand-side data.frame to the file
print( "writing RHS..." )
cat("RHS", file = mps.file, sep = "\n")
cat( sprintf( " %s %s %lf", rhs.frame$lab, rhs.frame$row, rhs.frame$bound ), file = mps.file, sep = "\n")

## write the bounds data.frame to the file
print( "writing BOUNDS..." )
cat("BOUNDS", file = mps.file, sep = "\n")
brfo <- bounds.frame[order( bounds.frame$col ),]
cat( sprintf( " %s %s %s", brfo$type, brfo$lab, brfo$col ), file = mps.file, sep = "\n")

cat("ENDATA", file = mps.file, sep = "\n")
close(mps.file)




################################################################################
## load the problem and generate a solution
lp <- lpx_read_freemps(mps.filename)
lpx_set_obj_dir(lp,LPX_MAX)

## just try to run the damn thing
lpx_simplex(lp)
obj.val <- lpx_get_obj_val( lp )

## or use the functions provided in schedpak.r
#row.report <- get.row.report( lp )
#col.report <- get.col.report( lp )

## pull the rows and objectives out...



## write out another file format that's easier to examine
lpx_write_cpxlp( lp, "danpickett.0.0.lp" )
lpx_delete_prob(lp);


## read in the solutions...

##nr1 <- c(20500,1025)
##nr2 <- c(19900,1550)
