\name{sp.sums.2}


\alias{sp.sums.2}

\title{
Generates species summaries from a dataframe that contains stem measurements.
}

\description{ This function generates a dataframe object that contains
the original stem measurements and appends columns named by
log.grades, where log.grades corresponds to the minimum log diameters
defined by log.breaks.
}


\usage{

sp.sums.2( x,
	   log.breaks=c(2,5,12,18,32,999),
           log.grades=c("pulp","s4","s3","s2","s1","peeler") )

}

\arguments{
  \item{x}{The dataframe containing the original stem measurements.}
  \item{log.breaks}{The vector defining the minimum diameters for the log grades defined by log.grades.}
  \item{log.grades}{The vector defining the text labels for the log.breaks.}
}

\value{
A dataframe that contains the original stem measurements with the log volumes appended for each species. 
}

\details{
The function generates summaries, by species, for common statistics used in growth and yield simulations. 
It performs the same function as the \code{sp.sums} function in the Rconifers library. 
}

\references{
  Robinson, A.P., and J.D. Hamann. 2010.  Forest Analytics with R: an
  Introduction. Springer.
}

\author{
  Jeff D. Hamann <jeff.hamann@forestinformatics.com>
}

\seealso{
\code{rconifers::sp.sums}
}

\examples{

# test <- data.frame(a = c(1,2), b = c(3,4))
# show.cols.with.na(test)
# is.na(test$a[2]) <- TRUE
# show.cols.with.na(test)

}

\keyword{ manip }



