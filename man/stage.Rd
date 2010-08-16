\name{stage}

\alias{stage}

\docType{data}

\title{
Stage's Grand fir (Abies grandis (Dougl) Lindl.) tree measurement data 
}

\description{
  The data are internal stem measures from 66 trees.  The trees were selected as
  having been dominant throughout their lives with no visible evidence
  of damage or forks. The trees came from stands throughout the inland
  range of the species.
}

\usage{data(stage)}

\format{
  A data frame with 542 observations on the following 11 variables.
  \describe{
    \item{\code{Tree.ID}}{A factor uniquely identifying the tree.}
    \item{\code{Forest}}{An integer distinguishing the forest.}
    \item{\code{HabType}}{An integer distinguishing the habitat type.}
    \item{\code{Decade}}{The decade of the trees life represented by the
  observation.}
    \item{\code{Dbhib}}{Diameter (in.) at 1.37 m (4'6'') inside bark.}
    \item{\code{Height}}{Height of tree (ft)}
    \item{\code{Age}}{Age of tree at felling}
    \item{\code{Forest.ID}}{The national forest in which the tree was felled.}
    \item{\code{Hab.ID}}{Daubenmmire's (1952) classification of the local growing conditions: \code{Ts/Pac} \code{Ts/Op} \code{Th/Pach} \code{AG/Pach} \code{PA/Pach}}
    \item{\code{dbhib.cm}}{Diameter (cm.) at 1.37 m (4'6'') inside bark.}
    \item{\code{height.m}}{Height of tree (m)}
  }
}

\details{

  Quoting Stage (1963), "After felling, the total height was measured,
  and the age at breast height was determined by counting the rings on a
  crossection. Then, decadal height growth was measured, starting from
  the termination of the 1956 height growth. The count of whorls was
  checked by a ring count at every decadal point. ...  Diameters at
  breast height (measuread along an average radius with a scale having
  10 graduations per half inch) corresponding to each decadal height
  point were also recorded."

  The national forests are: Kaniksu, Coeur d'Alene, St. Joe, Clearwater,
  Nez Perce, Clark Fork, Umatilla, Wallowa, and Payette.
  
}

\source{
Stage, A. 1963. A mathematical approach to polymorphic site index curves
for grand fir.  Forest Science 9(2) 167-180.
}

\references{
  Robinson, A.P., and J.D. Hamann. 2010.  Forest Analytics with R: an
  Introduction. Springer. 
}

\examples{
data(stage)
str(stage) 
}

\keyword{datasets}
