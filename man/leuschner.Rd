\name{leuschner}
\alias{leuschner}
\docType{data}
\title{ Leuschner harvest schedule yield data }
\description{
  These data are used to present and study forest regulation using mathematical programming and R.
}


\usage{data(leuschner)}
\format{
  A data frame with 48 observations on the following 4 variables.
  \describe{
    \item{stand}{index variable identifying the type of forested area}
    \item{period}{cutting period of the forest harvest schedule}
    \item{age}{at harvest at cutting period midpoint of planning period}
    \item{vol}{the stand volume at age, in thousands of cubic feet per acre}
  }
}
\details{
     This table contains example yields for an 84,000 acre (33993.93 ha)
    Douglas-fir forest. The yields span over six (6) planning periods and eight (8) age classes from age 25 
to 95 years of age.
    }
\source{
     The data were originally published in Leuschner (1990). 
}
\references{
  Leuschner, William A. 1990. Forest Regulation, Harvest Scheduling, and Planning Techniques.
  John Wiley and Sons, Inc.

  Robinson, A.P., and J.D. Hamann. 2010.  Forest Analytics with R: an
  Introduction. Springer. 
}

\examples{
data(leuschner)

print( leuschner )
}

\keyword{datasets}
