\name{show.cols.with.na}

\alias{show.cols.with.na}

\title{
Identifies variables in a dataframe that have missing values, along with a count.
}

\description{
This function reports the number of missing values in each variable in
a dataframe.  Variables that have no missing values are ignored.
}

\usage{
show.cols.with.na(x)
}

\arguments{
  \item{x}{
The dataframe to check.
  }
}

\value{
A vector of missing values counts, one value for each variable with
missing values.  If no variables have missing values then prints a
message and exits. 
}

\references{
  Robinson, A.P., and J.D. Hamann. 2010.  Forest Analytics with R: an
  Introduction. Springer.
}

\author{
  Andrew Robinson <apro@unimelb.edu.au>
}

\seealso{
\code{\link{is.na}}
}

\examples{

test <- data.frame(a = c(1,2), b = c(3,4))
show.cols.with.na(test)
is.na(test$a[2]) <- TRUE
show.cols.with.na(test)

}

\keyword{ manip }
