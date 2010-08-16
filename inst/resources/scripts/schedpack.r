#	$Id: schedpack.r 4099 2010-06-29 07:18:25Z hamannj $	


## this is a set of functions to help with the R book
## for harvest scheduling.

##    No.   Row name   St   Activity     Lower bound   Upper bound    Marginal
## ------ ------------ -- ------------- ------------- ------------- -------------
##      1 hvol1        NS             0             0             =       78.3347
##      2 hvol2        NS             0             0             =      -14.3923
##      3 hvol3        NS             0             0             =      -104.408

get.row.report <- function( lp ) {
  
  ## get the number of columns
##  ncols <- lpx_get_num_cols( lp )
  nrows <- lpx_get_num_rows( lp )
  
  ## build the table for the rows
  r <- NULL
  for( i in 1:nrows ) {
    ## actually, you'll want to use the row_names() as the rownames()
    ## I know, it sounds lame, but that's the way it's got to be... 

    r <- rbind( r,
               as.data.frame( cbind( lpx_get_row_name( lp, i ),
                                    lpx_get_row_stat( lp, i ),
                                    ##glpk_strerror(lpx_get_row_stat(lp,i)),
                                    lpx_get_row_prim( lp, i ),
                                    lpx_get_row_lb( lp, i ),
                                    lpx_get_row_ub( lp, i ),
                                    lpx_get_row_dual( lp, i ),
                                    ##lpx_get_row_b_ind( lp, i ),
                                    glpk_strerror(lpx_get_row_type(lp,i))
                                    )
                             )
               )

##     r <- rbind( r,
##                as.data.frame( cbind( lpx_get_row_name( lp, i ),
##                                     lpx_get_row_stat( lp, i ),
##                                     glpk_strerror(lpx_get_row_stat(lp,i)),
##                                     lpx_get_row_prim( lp, i ),
##                                     lpx_get_row_lb( lp, i ),
##                                     lpx_get_row_ub( lp, i ),
##                                     lpx_get_row_dual( lp, i ),
##                                     lpx_get_row_b_ind( lp, i ),
##                                     glpk_strerror(lpx_get_row_type(lp,i))
##                                     )
##                              )
##                )


  }
  
  
  ##names(r) <- c("name","status","activity","lb","ub","dual","b_ind","type" )
  ##names(r) <- c("name","status","prim","lb","ub","dual","b_ind","strerr" )
  names(r) <- c("name","status","prim","lb","ub","dual","strerr" )
  ## rownames( r ) <- r$name
  rownames( r ) <- 1:nrows

  r
}

  

#    No. Column name  St   Activity     Lower bound   Upper bound    Marginal
# ------ ------------ -- ------------- ------------- ------------- -------------
#      1 x(1)         B        109.153             0               
#      2 x(2)         NL             0             0                    -76.3559
#      3 x(3)         B            200             0               


get.col.report <- function( lp ) {

  ncols <- lpx_get_num_cols( lp )
##  nrows <- lpx_get_num_rows( lp )

  ## build the table for the columns
  c <- NULL
  for( i in 1:ncols ) {
    ## actually, you'll want to use the row_names() as the rownames()
    ## I know, it sounds lame, but that's the way it's got to be... 
    c <- rbind( c,
               as.data.frame( cbind( I(lpx_get_col_name( lp, i )),
                                    I(lpx_get_col_stat( lp, i ) ),
                                    I(lpx_get_col_prim( lp, i ) ),
                                    I(lpx_get_col_lb( lp, i ) ),
                                    I(lpx_get_col_ub( lp, i ) ),
                                    I(lpx_get_col_dual( lp, i ) ),
                                    I(lpx_get_col_b_ind( lp, i ) ),
                                    I(lpx_get_col_type( lp, i ) )
                                    )
                             )
               )
  }
  
  
  names(c) <- c("name","status","activity","lb","ub","dual","b_ind","type" )
  rownames( c ) <- 1:ncols

  c
}

  ##print( c )
  

################################################################################
################################################################################
## this is the rx generator (column generator)...
################################################################################
################################################################################


  
## this is the rx generator...
## R --slave --vanilla < R/rxgen.r $1
## this chunk is what you're going to add to the springer book

##rm( list=ls() )

## this function returns a list object that contains
## a set of N variables, where each variable can be
## a scalar, vector, or matrix of data you want to
## keep track of by polygon

## that the entry for the
## 
## what do these values mean?!
## p == period
## t == veg.type
## e == existing yields
## r == regeneration yields
## min.delay == number of periods between final harvests
## acres == area of the polygon
print( "defining polygon.rx.rows()" )
polygon.rx.rows <- function( p, t, e, r, min.delay, acres ) {

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

    if( n.cuts == 1 ) {
      y.e[cut.periods[1]] <- e[cut.periods[1],t]
      is.valid <- TRUE
    }

    ## if there's more than a single harvest
    if( n.cuts > 1 ) {
      y.e[cut.periods[1]] <- e[cut.periods[1],t]
      diff.periods <- diff( cut.periods )
      y.r[cut.periods[2:length(cut.periods)]] <- r[diff.periods,t]
      
      ## determine if there's enough delay between harvests
      ok.delays <- sum( diff.periods >= min.delay )
      
      if( ok.delays == length( diff.periods ) ) {
        is.valid <- TRUE
      } else {
        is.valid <- FALSE
      }
    }


    ## build the return structure
    ret.val[[i]]$mask <- mask[i,]
    ret.val[[i]]$n.cuts <- n.cuts
    ret.val[[i]]$cut.periods <- cut.periods ## these represent the columns that need to be gen'd    
    ret.val[[i]]$cut.e <- cut.periods[1]
    if( n.cuts > 1 ) {
      ret.val[[i]]$cut.r <- cut.periods[2:length(cut.periods)]
    } else {
      ret.val[[i]]$cut.r <- NA
    }  

    ret.val[[i]]$is.valid <- is.valid
    ret.val[[i]]$vol.e <- y.e * acres
    ret.val[[i]]$vol.r <-  y.r * acres
    ret.val[[i]]$vol <- ( y.e + y.r ) * acres
    ret.val[[i]]$act.acres <- mask[i,] * acres

    ## calculate the npv of the resulting volumes
    ret.val[[i]]$gross.sales.e <- y.e * 100 * 4 * acres
    ret.val[[i]]$gross.sales.r <- y.r * 100 * 2.5 * acres
    ret.val[[i]]$logging.costs.e <- y.e * 100 * 1 * acres
    ret.val[[i]]$logging.costs.r <- y.r * 100 * 0.75 * acres
    ret.val[[i]]$site.prep <- mask[i,] * 500 * acres
    ret.val[[i]]$admin <- rep( 1, p ) * 3 * 10 * acres
   
    ## assume all costs are applied in the middle of the period
    ## even for the 
    ret.val[[i]]$total.costs <- ret.val[[i]]$logging.costs.e  + 
      ret.val[[i]]$logging.costs.r + ret.val[[i]]$site.prep + 
        ret.val[[i]]$admin 

    ret.val[[i]]$net.rev <- ret.val[[i]]$gross.sales.e + ret.val[[i]]$gross.sales.r -
      ret.val[[i]]$total.costs
 
    yrs <- (1:p + 0.5) * 10 - 10
    ##print( yrs )

    ret.val[[i]]$disc <- ( 1 + 0.04 ) ^ (-yrs)
    ##print( ret.val[[i]]$disc )

    ret.val[[i]]$npv <- ret.val[[i]]$net.rev * ret.val[[i]]$disc
    ret.val[[i]]$sum.npv <- sum( ret.val[[i]]$npv )

  }

  ret.val

}


## extract the data and masks (using sapply)
## this function returns a set of columns of the variables of interest
print( "defining get.dcm()" )
get.dcm <- function( x ) {
  ret.val <- cbind( x$vol.e,
                   x$vol.r,
                   x$vol, 
                   x$total.cost,
                   x$net.rev,
                   x$npv, 
                   x$act.acres,
                   x$mask  )
  ret.val
}


## this function returns a dcm for the stand and prescription set
## which is an N x M variables matrix
## the format of the output is a set of rows and columns that 
## are used to generate the mps file
print( "defining stand.dcm()" )
stand.dcm <- function( id, n.periods, veg.type, ylds.m, regen.ylds.m, delay, acres ) {

  rx.rows <- polygon.rx.rows( per, veg.type, ylds.m, regen.ylds.m, delay, acres )
  rx.col <- sapply( rx.rows, get.dcm )
  
  is.valid <- sapply( rx.rows, function(x) x$is.valid )
  npvs <- sapply( rx.rows, function(x) x$sum.npv )
  rx.col <- rbind( rx.col, npvs )
  
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
               ## add other scalar or vector labels here
               ## for the variables you want to track over time
               )

  rownames( rx.col ) <- rowlabs
  
  valid.rxs <- rx.col[,is.valid]

  ## the column names define the decision variables (for now, there are 31 stands * 19 rx columns,
  ## plus the objective
  colnames( valid.rxs ) <- paste( "s", id, "rx", 1:(ncol(valid.rxs)), sep="" )
  valid.rxs
}





################################################################################
################################################################################
## functions from schedpak.r
################################################################################
################################################################################

## you need a function that returns the column(s)
## set the objective function to npv

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
##print( "defining polygon.rx.rows()" )
sb.polygon.rx.rows <- function( p,
                            veg.type,
                            e, ## are these supposed to be matricies!?
                            r, ## are these supposed to be matricies!?
                            min.delay,
                            acres,
                            irate ) {

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
    

    ## this should be an argument
    yrs <- (1:p + 0.5) * 10 - 10
    ret.val[[i]]$disc <- ( 1 + irate ) ^ (-yrs)
    ret.val[[i]]$npv <- ret.val[[i]]$net.rev * ret.val[[i]]$disc
    ret.val[[i]]$sum.npv <- sum( ret.val[[i]]$npv )

  }
    
  ret.val

}


## this function returns a dcm for the stand and prescription set
## which is an N x M variables matrix
## the format of the output is a set of rows and columns that 
## are used to generate the mps file
##print( "defining stand.dcm()" )
sb.stand.dcm <- function( id, n.periods, veg.type, ylds.m, regen.ylds.m, acres, irate ) {

  delay <- 0
  
  ## extract the data and masks (using sapply)
  ## this function returns a set of columns of the variables of interest
  print( "defining get.dcm()" )
  sb.get.dcm <- function( x ) {
    
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
  
  ## for the stand, generate a set of columns (for now as rows that will be transposed)
  ## that represent the outcomes from the set of prescriptions that can be generated
  ## as a set 2^{per} cut/nocut decisions.

  ##polygon.rx.rows( 7, 1, dp.ylds.m, dp.regen.ylds.m, 0, 1.0 )
  rx.rows <- sb.polygon.rx.rows( n.periods, veg.type, ylds.m, regen.ylds.m, delay, acres, irate )  
  rx.col <- sapply( rx.rows, sb.get.dcm )
  
  ## add the is.value check for the stand
  is.valid <- sapply( rx.rows, function(x) x$is.valid )

  ## add the npvs for the stand,rx
  npvs <- sapply( rx.rows, function(x) x$sum.npv )
  rx.col <- rbind( rx.col, npvs )
  
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
  ##valid.rxs <- rx.col[,is.valid]
  ##  print( valid.rxs )
  valid.rxs <- rx.col
  
  valid.rxs
}









################################################################################
## this function writes an MPS file for the four data.frame objects passed

print( "defining write.mps.file()" )
write.mps.file <- function( mps.filename, rows.frame, cols.frame, rhs.frame, bounds.frame ) {
  
################################################################################
  ## write out the freemps file.
  mps.file <- file( mps.filename, "w")  # open an output file connection
  cat( sprintf( "NAME %s", mps.filename ), file = mps.file, sep = "\n")
  
  ## write the rows data.frame to the file
  print( "writing ROWS..." )
  cat("ROWS", file = mps.file, sep = "\n")
  cat( sprintf( " %1s %s", rows.frame$dir, rows.frame$row ), 
      file = mps.file, sep = "\n")
  
  ## write the cols data.frame to the file
  print( "writing COLUMNS..." )
  cat("COLUMNS", file = mps.file, sep = "\n")
  ## rbind the cols.frames
  cf <- cols.frame[order( cols.frame$col ),]
  cf <- cf[cf$coeff != 0,]
  cf2 <- cf[order( cf$col ),]
  cat( sprintf( " %s %s %f", cf2$col, cf2$row, cf2$coeff ), 
      file = mps.file, sep = "\n")
  
  ## write the right-hand-side data.frame to the file
  print( "writing RHS..." )
  cat("RHS", file = mps.file, sep = "\n")
  cat( sprintf( " %s %s %f", rhs.frame$lab, rhs.frame$row, rhs.frame$bound ), 
      file = mps.file, sep = "\n")
  
  ## write the bounds data.frame to the file
  print( "writing BOUNDS..." )
  cat("BOUNDS", file = mps.file, sep = "\n")
  brfo <- bounds.frame[order( bounds.frame$col ),]
  cat( sprintf( " %s %s %s", brfo$type, brfo$lab, brfo$col ), 
      file = mps.file, sep = "\n")
  
  cat("ENDATA", file = mps.file, sep = "\n")
  close(mps.file)
  
}



################################################################################
## read a glpk solution file into a list object

################################################################################
## import the mip solution from glpk into rufus
##print( "read the solution file..." )


print( "defining read.sln.file()" )
read.sln.file <- function( filename ) {

sln.file <- file( filename, "r")  # open an output file connection

rl <- readLines( sln.file, n=1 )

soln <- list()

soln$soln.nrows <- as.numeric( strsplit( rl, " " )[[1]] )[1]
soln$soln.ncols <- as.numeric( strsplit( rl, " " )[[1]] )[2]

## if( soln.nrows != nrow( rows.frame ) ) {
##   stop( "whoa!" )
## }

## get the solver status and the value of the objective function
so <- readLines( sln.file, n=1 )
soln$solver.status <- as.numeric( strsplit( so, " " )[[1]] )[1]
soln$obj.val <- as.numeric( strsplit( so, " " )[[1]] )[2]

## read the entries for the rows (accounting/auxilary variables)
soln$soln.rows <- as.numeric( readLines( sln.file, nrow( rows.frame ) ) )

## read the decision vector portion of the file
soln$soln.cols <- as.numeric( readLines( sln.file, soln$soln.ncols ) )

close(sln.file)

## assign names to the decision variables (forget about the harvest volumes at the end for now)
##names( soln.cols ) <- paste( "s", kronecker( 1:(nrow(stands)), rep( 1, 2^n.periods ) ), "rx", 1:(2^n.periods), sep="" )
##names( soln.rows ) <- rows.frame$row

soln

}



print( "defining evenflow.constraints()" )
evenflow.constraints <- function( efdev ) {

  ## create a blank list object to hold the data.frames
  constr <- list()
  
##   efc <- evenflow.constraints( efdev )
  
##   ## add the entries to the rows, cols, and rhs frames
##   rows.frame <- rbind( rows.frame, rows.frame.evenflow )
##   cols.frame <- rbind( cols.frame, cols.frame.evenflow )
##   rhs.frame <- rbind( rhs.frame, rhs.frame.evenflow )

  print( "building DCM entries for even flow constraints... begin" )


## forest level constraints
##efdev <- 0.2 ## +/- twenty percent harvest volume
##efdev <- 0.5 ## +/- ten percent harvest volume

  rows.frame.evenflow <- NULL
  cols.frame.evenflow <- NULL
  rhs.frame.evenflow <- NULL
  
  print( "creating ROWS and RHS for even flow constraints..." )
  for( i in 2:n.periods ) {

    ## add the rows (constraints) for the evenflow ratios
    ##  G L12 >= 0
    rows.frame.evenflow <- rbind( rows.frame.evenflow,
                                 data.frame(
                                            row=sprintf( "L%d%d", i-1, i ),
                                            dir = "G" )
                                 )        
    ##  G L21 >= 0
    rhs.frame.evenflow <- rbind( rhs.frame.evenflow,
                                data.frame(
                                           lab=sprintf( "RHS1" ),
                                           row=sprintf( "L%d%d", i-1, i ),
                                           bound=0 )
                                )

    ##  L U21 <= 0
    rows.frame.evenflow <- rbind( rows.frame.evenflow,
                                 data.frame(
                                            row=sprintf( "U%d%d", i-1, i ),
                                            dir = "L" )
                                 )        
    
    ##  L U21 <= 0
    rhs.frame.evenflow <- rbind( rhs.frame.evenflow,
                                data.frame(
                                           lab=sprintf( "RHS1" ),
                                           row=sprintf( "U%d%d", i-1, i ),
                                           bound=0 )
                                )

##   }
  
##   print( "creating COLUMNS for even flow constraints" )
##   for( i in 2:(n.periods) ) {    
    
    ## col, row, coeff
    ##  each period needs a set of constraints in the form of:
    ##  hvol3 L32 1
    ##  hvol3 U32 1
    ##  hvol3 L43 -0.8
    ##  hvol3 U43 -1.2

    ## lower bounds for the harvest volumes
    cols.frame.evenflow <- rbind( cols.frame.evenflow,
                                 data.frame(
                                            col=sprintf( "hvolt%d", i-1 ),
                                            row=sprintf( "L%d%d", i-1, i ),
                                            coeff = -(1.0 - efdev) )
                                 )

    cols.frame.evenflow <- rbind( cols.frame.evenflow,
                                 data.frame(
                                            col=sprintf( "hvolt%d", i ),
                                            row=sprintf( "L%d%d", i-1, i ),
                                            coeff = 1 )
                                 )
    
    ## these are the upper bounds for the harvest volumes
    cols.frame.evenflow <- rbind( cols.frame.evenflow,
                                 data.frame(
                                            col=sprintf( "hvolt%d", i-1 ),
                                            row=sprintf( "U%d%d", i-1, i ),
                                            coeff = -(1.0 + efdev) )
                                 )
    
    cols.frame.evenflow <- rbind( cols.frame.evenflow,
                                 data.frame(
                                            col=sprintf( "hvolt%d", i ),
                                            row=sprintf( "U%d%d", i-1, i ),
                                            coeff = 1 )
                                 )
    
  }
    
  ## add the entries to the rows, cols, and rhs frames
##   constr$rows.frame <- rbind( rows.frame, rows.frame.evenflow )
##   constr$cols.frame <- rbind( cols.frame, cols.frame.evenflow )
##   constr$rhs.frame <- rbind( rhs.frame, rhs.frame.evenflow )

  constr$rows.frame <- rows.frame.evenflow
  constr$cols.frame <- cols.frame.evenflow
  constr$rhs.frame <- rhs.frame.evenflow

  print( "building DCM entries for even flow constraints... complete" )  

  constr
  
}



## bit of code from the sweave document to keep it clean.
## % Convert the adjacency list to an adjacency matrix (a little detail
## % about an adjacency graph.

## % <<echo=true>>=

## % ## create an adjacency matrix for this problem
## % names( adj ) <- c("unit","adj" )
## % adj.m <- matrix( 0, max( adj$unit ), max( adj$unit ) )

## % for( i in 1:nrow(adj) ) {
## %   adj.m[adj[i,]$unit,adj[i,]$adj] <- 1
## % }

## % adj.deg <- rowSums( adj.m ) 
## % adj.p <- adj.m[1:nrow(adj.m),] / adj.deg

## % @ 


## % If we formulate our model as a mixed-integer program to account for
## % adjacency constraints, we can then track exactly what management units
## % are harvested in each period by plotting a map of the We have also
## % generated an adjacency matrix from the geographic data. Modify the
## % previous problem and then solve using mixed-integer programming. The
## % mixed interger programming examples will come from either your notes
## % from strategic and tactical planning, or from leuschener (year). We
## % now turn our attention to the problem of adjacency constraints. For
## % this we will need a map of the landscape from which we can derive a
## % set of equations that will yield our constraints. We can now add the
## % adjacency constraints to the problem by adding the rows for each of
## % the stand-period combinations.

## % The set of matrices that are required for each of the periods can be
## % constructed by assigning the degree of the vertex to the diagonal of
## % the adjacency matrix

## % \begin{equation}
## %   \label{eq:dp.adj.m}
## %   \mathbf{A}_{lp} = \mathbf{A} + D(\mathbf{A})
## % \end{equation}

## % which can be constructed as

## % <<echo=true,eval=false>>=
## % lp.adj.m <- adj.m + diag( adj.deg )
## % @ 

## % For this problem, we restrict the harvest activity such that a no two
## % adjacent stands can be harvested in the same decade. This constraint
## % can be expressed using the following equation, for two adjacent units

## % \begin{equation}
## %   \label{eq:dp_adjacency_constraint_1}
## %   X_{ij} + \sum_{j=1}^{n}X_{jk} \le 1
## % \end{equation}

## % \noindent where $X_{ij}$ is a binary decision variable representing
## % the harvest of planning unit $i$ in period $j$ and
## % $\sum_{j=1}^{n}X_{jk}$ represents the binary decision variables to
## % harvest the $j$ adjacent units in period $k$. In this example, the
## % adjacency constraint equation for stand nine would be written as

## % \begin{equation}
## %   \label{eq:dp_adjacency_constraint_2}
## %   X_{9,1} + X_{2,1} + X_{3,1} + X_{14,1} + X_{17,1} \le 1
## % \end{equation}

## % \noindent which allows that only one unit from this set of possible
## % choices may be selected in the first period. Of course, for all seven
## % periods, the set of equations would be

## % \begin{eqnarray}
## %   X_{9,1} + X_{2,1} + X_{3,1} + X_{14,1} + X_{17,1} & \le & 1 \\
## %   X_{9,2} + X_{2,2} + X_{3,2} + X_{14,2} + X_{17,2} & \le & 1 \\
## %   X_{9,3} + X_{2,3} + X_{3,3} + X_{14,3} + X_{17,3} & \le & 1 \\
## %   X_{9,4} + X_{2,4} + X_{3,4} + X_{14,4} + X_{17,4} & \le & 1 \\
## %   X_{9,5} + X_{2,5} + X_{3,5} + X_{14,5} + X_{17,5} & \le & 1 \\
## %   X_{9,6} + X_{2,6} + X_{3,6} + X_{14,6} + X_{17,6} & \le & 1 \\
## %   X_{9,7} + X_{2,7} + X_{3,7} + X_{14,7} + X_{17,7} & \le & 1
## % \end{eqnarray}

## % \noindent which yields the adjacency constraints for only the first
## % harvest unit. As you might guess, the number of equations can become
## % large with the number of harvest units. As before, we can also
## % control the number of acres harvested by using constraints such as

## % \begin{equation}
## %   \alpha_{1} X_{1,1} + \alpha_{2} X_{2,1} + \hdots + \alpha_{n}
## %   X_{n,1} \ge A_{min}
## % \end{equation}

## % \noindent and

## % \begin{equation}
## %   \alpha_{1} X_{1,1} + \alpha_{2} X_{2,1} + \hdots + \alpha_{n}
## %   X_{n,1} \le A_{max}
## % \end{equation}

## % \noindent where $A_{min}$ and $A_{max}$ are the allowable minimum and
## % maximum harvest acres in each period and $\alpha_{a}, \hdots,
## % \alpha_{n}$  are the areas of the treatment units. 

## % We generate the detached coefficients matrix for every possible
## % perscription in our perscription table constructed from the
## % \texttt{bincombinations} function. 

## % \textbf{this isn't working either.}

## % <<eval=true,echo=false>>=

## % ## if you want to include adjacency constraints
## % if( include.adj ) {

## %   print( "building DCM entries for adjacency constraints... begin" )
  
## %   per.adj.rowlabs <- NULL
## %   for( i in 1:nrow( stands )) {
## %     per.adj.rowlabs <- c( per.adj.rowlabs, paste( "s", i, "p", 1:n.periods, sep="" ) )
## %   }
  
## %   print( "creating RHS for period adjacency" )
## %   adj.deg <- rowSums( adj.m )  ## do you really need this?
## %   per.adj.rhs.frame <- data.frame( lab=rep( "RHS1", nrow( stands ) ),
## %                                   row=per.adj.rowlabs,
## %                                   bound=kronecker( adj.deg, rep( 1, n.periods ) )
## %                                   )
  
## %   ## build the ROWS frame for the adj constraint
## %   ## you only need to do this once over all stands and periods
## %   ## becuase you already have the rows defined
## %   ## (good) do not modify
## %   print( "creating ROWS for period adjacency" )
## %   per.adj.rows.frame <- data.frame( row=per.adj.rowlabs, dir="L" )
  
## %   ## build the coeffs.frame for the adj constraints...
## %   ## (good) do not modify
## %   print( "creating COLUMNS for period adjacency" )
## %   n.stands <- nrow( stands )
## %   ad <- adj.m + diag( adj.deg )
## %   per.adj.coeffs.frame <- kronecker( ad, t( bincombinations( n.periods ) ) )
## %   rownames( per.adj.coeffs.frame ) <- per.adj.rowlabs
## %   colnames( per.adj.coeffs.frame ) <- paste( "s", kronecker( 1:n.stands, rep( 1, 2^n.periods ) ), "rx", 1:(2^n.periods), sep="" )

## %   per.adj.cols.frame <- NULL
## %   for( j in 1:ncol( per.adj.coeffs.frame ) ) {  
## %     rcc.frame <- data.frame(row   = names( per.adj.coeffs.frame[,j] ),
## %                             col  = colnames(per.adj.coeffs.frame)[j], 
## %                             coeff = per.adj.coeffs.frame[,j] )

## %     per.adj.cols.frame <- rbind( per.adj.cols.frame, rcc.frame )

## %     print( colnames(per.adj.coeffs.frame)[j] )
## %   }

## %   ## you don't need to build the bounds since there are no additional columns
## %   print( "building DCM entries for adjacency constraints... complete" )

## %   print( "appending the DCM entries for adjacency constraints..." )
## %   rhs.frame <- rbind( rhs.frame, per.adj.rhs.frame )
## %   rows.frame <- rbind( rows.frame, per.adj.rows.frame )
## %   cols.frame <- rbind( cols.frame, per.adj.cols.frame )
  
## % }

## % @ 




## % \begin{verbatim}
## %   ## definitions
## %   ## hvole == harvest volume of existing stands in period i
## %   ## hvolr == harvest volume of regen stands in period i
## %   ## hvolt == harvest volume (total) in period i
## %   ## tc == total operating costs in period i
## %   ## nr == net revenue in period i
## %   ## npvc == net present value component TODO: define this...
## %   ## hac == habitat acres? TODO: define
## %   ## cut == is.cut in period i (used for tracking the number of units to manage in period i -- admin)
## % \end{verbatim}

## % Since part of our analysis will be to examine the results of our
## % optimization, we want to plot harvest volumes, cash flows, and the
## % number of harvest units being cut in each period, by period for the
## % entire landscape. 

## % In the \texttt{schedpak.r} script we include three functions. The
## % \texttt{polygon.rx.rows()} is used to generate a set of prescriptions
## % (columns), using the \texttt{bincombinations()} function to generate
## % harvest timings, harvest volumes, total costs, discounted cash flows,
## % net present values (NPV) for each activity schedule.

## % that for each set of harvest sequences, named \texttt{polygon.rx.rows}
## % that returns a set of forecast forest conditions, from the current
## % condition of the stand, and some function arguments.





################################################################################
################################################################################
## this is code that generates the problem for the model 1 dan pickett
## for all the periods, using the bincombinations templates
################################################################################
################################################################################




## this is the code to create the data for the dp problem...








## ################################################################################
## ## import the mip solution from glpk into rufus
## ##print( "read the solution file..." )
## sln.filename <- "danpickett.1.sln"
## sln.file <- file( sln.filename, "r")  # open an output file connection

## rl <- readLines( sln.file, n=1 )
## soln.nrows <- as.numeric( strsplit( rl, " " )[[1]] )[1]
## soln.ncols <- as.numeric( strsplit( rl, " " )[[1]] )[2]

## if( soln.nrows != nrow( rows.frame ) ) {
##   stop( "whoa!" )
## }

## ## get the solver status and the value of the objective function
## so <- readLines( sln.file, n=1 )
## solver.status <- as.numeric( strsplit( so, " " )[[1]] )[1]
## obj.val <- as.numeric( strsplit( so, " " )[[1]] )[2]

## ## read the entries for the rows (accounting/auxilary variables)
## soln.rows <- as.numeric( readLines( sln.file, nrow( rows.frame ) ) )

## ## read the decision vector portion of the file
## soln.cols <- as.numeric( readLines( sln.file, soln.ncols ) )

## close(sln.file)

## ## assign names to the decision variables (forget about the harvest volumes at the end for now)
## names( soln.cols ) <- paste( "s", kronecker( 1:(nrow(stands)), rep( 1, 2^n.periods ) ), "rx", 1:(2^n.periods), sep="" )
## names( soln.rows ) <- rows.frame$row
