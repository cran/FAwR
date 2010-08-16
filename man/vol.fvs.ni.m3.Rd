\name{vol.fvs.ni.m3}

\alias{vol.fvs.ni.bdft}
\alias{vol.fvs.ni.m3}

\title{
  Predicts volume (m^3) from diameter (cm) and height (m) by species, using
  functions from Wykoff et al. (1982).
}

\description{
  This vectorized function uses models and parameter
  estimates from Wykoff et al. (1982) to predict tree volumes given tree
  heights (m) and over-bark diameter measurements (cm) taken at 1.37 m
  (4'6'').  The function vol.fvs.ni.bdft performs the computation in
  imperial units, and vol.fvs.ni.m3 is a wrapper for convenience.
}

\usage{
vol.fvs.ni.m3(spp, dbh.cm, ht.m)
vol.fvs.ni.bdft(spp, dbh.in, ht.ft)
}

%- maybe also 'usage' for other objects documented here.
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
  \item{ht.m}{
Tree height, m.
}
  \item{ht.ft}{
Tree height, ft.
}
}

\details{
  The species are: WP = white pine, WL = western larch, DF =
  Douglas-fir, GF = grand fir, WH = western hemlock, WC = western red
  cedar, LP = lodgepole pine, ES = Engelmann spruce, SF = subalpine fir,
  PP = ponderosa pine, and MH = mountain hemlock.


}

\value{
The function returns a vector of tree volumes, in cubic metres.
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

vol.fvs.ni.m3(c("DF, WH"), c(25, 27), c(15, 20))

}

\keyword{ manip }
