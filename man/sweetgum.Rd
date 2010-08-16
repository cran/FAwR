\name{sweetgum}

\alias{sweetgum}

\docType{data}

\title{
Lenhart's sweetgum (Liquidambar styraciflua L.) tree measurement data.
}

\description{
The original data, also supplied in the package, are measures of stem
diameter and height at various locations on the bole of 39 sweetgum
trees from Texas, USA. 
}

\usage{data(sweetgum)}

\format{
  A data frame with 39 tree-level observations on the following 8 variables.
  \describe{
    \item{\code{plot}}{Identifies the plot in which the tree was felled.}
    \item{\code{tree}}{A factor that identifies the tree.}
    \item{\code{dbh.in}}{The tree diameter (in.) at 1.37 m (4'6'')}
    \item{\code{stump.ht.ft}}{The height of the stump after felling (ft).}
    \item{\code{height.ft}}{The height of the tree (ft).}
    \item{\code{height.m}}{The height of the tree (m).}
    \item{\code{dbh.cm}}{The tree diameter (cm.) at 1.37 m (4'6'')}
    \item{\code{vol.m3}}{The estimated biological volume of the stem
  (cubic meters).}
  }
}

\details{

  The height measure is of the standing tree, so it includes the stump
  height.  The volume was computed by fitting a spline to the height and
  diameter measures, and integrating the solid that corresponds to the
  profile of the spline.  See Section 2.4.2 of Robinson and Hamann
  (2010) for more details.

}

\source{
The data were kindly supplied by Professor Timothy Gregoire and
were originally collected by David Lenhart.
}

\references{
  Robinson, A.P., and J.D. Hamann. 2010.  Forest Analytics with R: an
  Introduction. Springer. 
}

\examples{
data(sweetgum)
str(sweetgum)
}

\keyword{datasets}
