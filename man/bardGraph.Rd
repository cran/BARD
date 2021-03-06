\name{nb2graph}
\alias{nb2graph}
\alias{neighbors.nb}
\alias{neighbors}
\alias{n.comp.include}
\title{Neighborhood list helper functions}
\description{
These functions operate on neighbood lists generated by the spdep package.
}
\usage{
nb2graph(nb)
neighbors(nb,i,...)
neighbors.nb(nb,i,...)
n.comp.include(nb,include)
}

\arguments{
 \item{nb}{a neighborhood list generated by the spdep package}
 \item{i}{block id}
 \item{include}{exlusion list}
 \item{\dots}{additional arguments to pass onto neighbor methods}
}

\details{
These are primarily bard internal functions, exposed for programmers.

Neighbors is a generic method returning a list of indexes of all blocks directly conneccted to those in the supplied blocks id list.

nb2graph converts a \cite{spdep} neighborhood object into a graph object suitable for use with \cite{rbgl}

n.comp.exclude is analogous to \code{n.comp.nb} in that it returns the number of connected subgraphs inducd by the neighborhood graph. However, it allows an exlcusion list to be supplied, so that only a subset of the list is examined. This is semantically equivalent to \code{n.comp.nb(subset(nb,i))} but is roughly 10-50x faster.

}

\value{
  nb2graph returns a graph object, the others return a vector block ids
}

\author{ 
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah_altman/} 
 }


\seealso{ 
  \code{\link[spdep]{n.comp.nb}}
 }
 
\examples{
  data(suffolk.map)
  snb<-suffolk.map$nb
  neighbors(snb,c(1,2,3))
  kplan <- createKmeansPlan(suffolk.map,5)
  rplan <- createRandomPlan(suffolk.map,5)
  n.comp.include(snb,rplan==1)
  n.comp.include(snb,kplan==1)
}

\keyword{ IO }
\keyword{ spatial }
