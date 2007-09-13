\name{miniball}
\alias{miniball}
\title{compute minimum/bounding ball/circle}
\description{
Will compute the minimum ball (aka, bounding ball, bounding circle, minimum circle, spheroid hull) of a set of points.
}
\usage{
	miniball (points, pivot=TRUE, distances=FALSE)
}

\arguments{
	\item{points}{A \emph{n} by \emph{r} matrix of \emph{r}-dimensional points.}
  \item{pivot}{Use pivoting methods for numerical stability}
  \item{distances}{Return vector of distances of points from center}	
}

\details{
For those writing their own BARD score functions only.	Returns the center and squared radius of the ball, support points, distances and tolerances as a list.
}

\references{
B. Gartner, 1999, "Fast and robust smallest enclosing balls", In \emph{Proc.  ESA. '99}}


\examples{

# simple example
xy <- matrix(runif(50),25)
plot(xy)
mb <- miniball(xy)

#distances from point to center
dc <- rowSums(t(t(xy)-mb$center)^2)

}

\author{
Micah Altman
\email{Micah\_Altman@harvard.edu} (R interfaces)

\url{http://www.hmdc.harvard.edu/micah_altman/}  [R Interfaces]
(Miniball C++ code by B. Gartner)
}

\keyword{spatial}
\keyword{cluster}
