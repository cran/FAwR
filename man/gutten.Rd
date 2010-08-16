\name{gutten}

\alias{gutten}

\docType{data}

\title{
von Guttenberg's Norway spruce (Picea abies [L.] Karst) tree measurement data.
}

\description{
The data are measures from 107 trees.  The trees were selected as
being of average size from healthy and well stocked stands in the Alps. 
}

\usage{data(gutten)}
\format{
  A data frame with 1200 observations on the following 9 variables.
  \describe{
    \item{\code{site}}{Growth quality class of the tree's habitat.  5 levels.}
    \item{\code{location}}{Distinguishes tree location.  7 levels.}
    \item{\code{tree}}{An identifier for the tree within location.}
    \item{\code{age.base}}{The tree age taken at ground level.}
    \item{\code{height}}{Tree height, m.}
    \item{\code{dbh.cm}}{Tree diameter, cm.}
    \item{\code{volume}}{Tree volume.}
    \item{\code{age.bh}}{Tree age taken at 1.3 m.}
    \item{\code{tree.ID}}{A factor uniquely identifying the tree.}
  }
}

%%\details{
%%%%  ~~ If necessary, more details than the __description__ above ~~
%%}

\source{
  The data were kindly provided to us by Professor Boris Zeide,
  and are documented in von Guttenberg, A. R., 1915. Growth and yield of
  spruce in Hochgebirge. Franz Deuticke, Vienna (in German).
}

\references{
  Robinson, A.P., and J.D. Hamann. 2010.  Forest Analytics with R: an
  Introduction. Springer. 

  Zeide, B., 1993. Analysis of growth equations. Forest Science 39 (3),
  549-616
}

\examples{
data(gutten)
str(gutten) 
}

\keyword{datasets}
