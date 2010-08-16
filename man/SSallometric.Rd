\name{SSallometric}
\alias{SSallometric}

\title{
Self-starting version of the allometric function y = a x^b.
}

\description{
This function is used for fitting the allometric function to bivaraite
data.  The function is of the form y = alpha x ^ beta, where
traditionally x and y are different dimensions of a sampling unit, such
as diameter and volume of a tree.  
}

\usage{
SSallometric(x, alpha, beta)
}

\arguments{
  \item{x}{
predictor variable (e.g. volume)
}
  \item{alpha}{
product parameter
}
  \item{beta}{
exponent parameter
}
}

%%\details{
%%%%  ~~ If necessary, more details than the description above ~~
%%}

\value{

  The function returns a numeric vector of response variables.  The
  vector has two additional attributes: the gradient, which reports the
  first derivative of the function with respect to the parameters,
  evaluated at the response value; and the hessian, which reports the
  matrix of second partial derivatives of the function with respect to
  the parameters evaluated at the response variable.
}

\references{
Robinson, A.P., and J.D. Hamann. 2010.  Forest Analytics with R: an
Introduction. Springer. 
}

\author{
Andrew Robinson <apro@unimelb.edu.au>
}

\note{
Demonstrates construction and usage of self-starting functions using selfStart.
}

\seealso{
\code{\link{selfStart}}
}

\examples{
SSallometric(10, 2, 3)

data(sweetgum)
nls(vol.m3 ~ SSallometric(dbh.cm, alpha, beta),  data = sweetgum)
}

\keyword{ models }

