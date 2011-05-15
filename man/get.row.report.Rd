\name{get.row.report}


\alias{get.row.report}

\title{
Obtains row information about the GLPK problem object.
}

\description{
The function generates a \code{\link{data.frame}} object that contains row information from a GLPK object. 
}


\usage{
get.row.report( lp )
}

\arguments{
  \item{lp}{A GLPK object.}
}

\value{
A data.frame that contains several elements returned from GLPK wrapper functions that return attributes from a row from an GLPK object.
}

\details{
The function generates a \code{\link{data.frame}} object that contains row information from a GLPK object. 
% The functions that are called include: \code{\link{lpx_get_row_name}},
% \code{\link{lpx_get_row_stat}},
% \code{\link{lpx_get_row_prim}},
% \code{\link{lpx_get_row_lb}},
% \code{\link{lpx_get_row_ub}},
% \code{\link{lpx_get_row_dual}},
% \code{\link{glpk_strerror}}, and
% \code{\link{lpx_get_row_type}}

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
\code{\link{get.col.report}}
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



