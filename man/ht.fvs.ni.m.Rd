\name{ht.fvs.ni.m}

\alias{ht.fvs.ni.ft}
\alias{ht.fvs.ni.m}

\title{
  Predicts height (m) from diameter (cm) by species, using
  functions from Wykoff et al. (1982).
}

\description{
  This vectorized function uses models and parameter
  estimates from Wykoff et al. (1982) to predict tree heights given
  over-bark diameter measurements (cm) taken at 1.37 m (4'6'').  The function
  ht.fvs.ni.ft performs the computation in imperial units, and
  ht.fvs.ni.m is a wrapper for convenience.
}

\usage{
ht.fvs.ni.ft(spp, dbh.in)
ht.fvs.ni.m(spp, dbh.cm)
}

\arguments{
  \item{spp}{
Tree species.  Must be one of: WP, WL, DF, GF, WH, WC, LP, ES, SF, PP, MH
}
  \item{dbh.cm}{
Tree diameter, cm, measured at 1.37 m. from the ground.
}
  \item{dbh.in}{
Tree diameter, in., measured at 1.37 m. from the ground.
}
}

\details{

  The species are: WP = white pine, WL = western larch, DF =
  Douglas-fir, GF = grand fir, WH = western hemlock, WC = western red
  cedar, LP = lodgepole pine, ES = Engelmann spruce, SF = subalpine fir,
  PP = ponderosa pine, and MH = mountain hemlock.
  
  The function was fitted using least squares regression; the resposne
 variable was log of height and the predictor variable 1 / (DBH + 1).
}

\value{
The function returns a vector of tree heights, in metres.
}

\references{
  Robinson, A.P., and J.D. Hamann. 2010.  Forest Analytics with R: an
  Introduction. Springer.

  Wykoff, W. R., Crookston, N. L., Stage, A. R., 1982. User's Guide to the
  Stand Prognosis Model. GTR-INT 133, USDA Forest Service, Ogden, UT.
}

\author{
  Andrew Robinson <apro@unimelb.edu.au>
}

\note{
The function doesn't perform any input checks.  Caveat computator.
}

\examples{
ht.fvs.ni.m(c("DF, WH"), c(25, 27))
}

\keyword{ manip }
