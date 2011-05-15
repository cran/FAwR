\name{get.col.report}


\alias{get.col.report}

\title{
Obtains column information about the GLPK problem object.
}

\description{This function returns a data.frame object that contains several useful column attributes about a linear programming GLPK object.
}


\usage{
get.col.report( lp )
}

\arguments{
  \item{lp}{A GLPK object.}
}

\value{
A data.frame that contains several elements returned from GLPK wrapper functions that return attributes from a column from an GLPK object.
}

\details{
% The function generates a \code{\link{data.frame}} object that contains row information from a GLPK object. 

The function generates a \code{\link{data.frame}} object that contains information from a GLPK object. 

% The functions calls several functions in the \code{\link{glpk}} package. 

}

\references{
  Robinson, A.P., and J.D. Hamann. 2010.  Forest Analytics with R: an
  Introduction. Springer.

  The GNU GLPK home page at http://www.gnu.org/software/glpk/glpk.html
}

\author{
  Jeff D. Hamann <jeff.hamann@forestinformatics.com>
}

\seealso{
\code{get.row.report}
}


% \examples{

%   ## load the leuschner lp problem object
%   library( glpk )
%   lp <- lpx_read_cpxlp( "../data/leuschner.xlp" )
%   lpx_simplex(lp)
%   source( "../resources/scripts/schedpack.r" )
%   row.rpt <- get.row.report( lp )
%   print( row.rpt )

% }



\keyword{ manip }



