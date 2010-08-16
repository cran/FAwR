\name{ufc}
\alias{ufc}
\docType{data}
\title{ Upper Flat Creek forest cruise tree data }
\description{
  These are a subset of the tree measurement data from the
  Upper Flat Creek unit of the University of Idaho Experimental
  Forest, which was measured in 1991. 
}
\usage{data(ufc)}
\format{
  A data frame with 336 observations on the following 5 variables.
  \describe{
    \item{plot}{plot label}
    \item{tree}{tree label}
    \item{species}{species kbd with levels \kbd{DF}, \kbd{GF}, \kbd{WC}, \kbd{WL}}
    \item{dbh.cm}{tree diameter at 1.37 m. from the ground, measured in
          centimetres.}
    \item{height.m}{tree height measured in metres}
  }
}
\details{
     The inventory was based on variable radius plots with 6.43 sq. m.
     per ha. BAF (Basal Area Factor).  The forest stand was 121.5 ha.
     This version of the data omits errors, trees with missing heights,
     and uncommon species.  The four species are Douglas-fir, grand fir,
     western red cedar, and western larch.
}
\source{
     The data are provided courtesy of Harold Osborne and Ross
     Appelgren of the University of Idaho Experimental Forest. 
}
\references{
  Robinson, A.P., and J.D. Hamann. 2010.  Forest Analytics with R: an
  Introduction. Springer. 
}
\examples{
data(ufc)
}

\keyword{datasets}
