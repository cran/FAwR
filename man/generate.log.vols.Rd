\name{generate.log.vols}

\alias{generate.log.vols}

\title{
Generates log volumes given a dataframe that contains stem measurements.
}

\description{ This function generates a dataframe object that contains
the original stem measurements and appends columns named by
log.grades, where log.grades corresponds to the minimum log diameters
defined by log.breaks.
}


\usage{
generate.log.vols( x,
		   log.breaks=c(2,5,12,18,32,999),
                   log.grades=c("pulp","s4","s3","s2","s1","peeler"),
                   display.stems=FALSE	 )
}

\arguments{
  \item{x}{The dataframe containing the original stem measurements.}
  \item{log.breaks}{The vector defining the minimum diameters for the log grades defined by log.grades.}
  \item{log.grades}{The vector defining the text labels for the log.breaks.}
  \item{display.stems}{The control switch to display stems as they are being merchanised.}
}

\value{
A dataframe that contains the original stem measurements with the log volumes appended. 
}

\details{
The function contains supporting function to determine the merchantable stem height, then tallies the amount of Smalian log volume, 
in each of the log.grades. The resulting dataframe is then converted to be appended to each tree record in x. Normally, it would not be called directly. 
}

\references{
  Robinson, A.P., and J.D. Hamann. 2010.  Forest Analytics with R: an
  Introduction. Springer.
}

\author{
  Jeff D. Hamann <jeff.hamann@forestinformatics.com>
}

\seealso{
\code{\link{is.na}}
}

\examples{


# test <- data.frame(a = c(1,2), b = c(3,4))
# show.cols.with.na(test)
# is.na(test$a[2]) <- TRUE
# show.cols.with.na(test)

}

\keyword{ manip }



