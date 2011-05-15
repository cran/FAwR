\name{herbdata}
\alias{herbdata}
\docType{data}
\title{ Herbicide trial seedling data }
\description{
  These data were collected during the 1994 planting season in Southwest Washington State. 
}
\usage{data(herbdata)}
\format{

  A data frame with 961 observations on the following 8 variables.
  \describe{
    \item{treat}{treatment for control or application}
    \item{rep}{replication}
    \item{tree}{tree number in the replication}
    \item{date}{date the observation was made}
    \item{isalive}{indicator for alive/dead status}
    \item{height}{height of the tree in cm}
    \item{dia}{basal diameter of the stem, in mm}
    \item{dbh}{diameter at breast height, in mm}
  }
}

\details{

The plots were installed during the 1994 planting season in
southwestern Washington by Don Wallace and Bruce Alber. Three
replications of 20 seedlings were planted in two blocks. The two
blocks were a control block and a block treated with 220 ml per
hectare of Oust herbicide.  The plots were then
measured over the next ten years.  At each observation, the basal
diameter, total height, and condition of the stem were recorded.  When
the stems reached breast height (1.37 m in the United States), the
breast height diameter was also recorded.  An indicator variable was
used to record if the stem was dead or alive. If the stem was dead,
the observations were recorded as NA.  
}

\source{
     The data are provided courtesy of Don Wallace and Bruce Alber of the Wilbur-Ellis Company. 
}

\references{
  Robinson, A.P., and J.D. Hamann. 2010.  Forest Analytics with R: an
  Introduction. Springer. 
}
\examples{
data(herbdata)
}

\keyword{datasets}
