\name{basem<-.default}
\alias{basem<-.default}
\alias{basem.default}
\alias{basem<-}
\alias{basem}
\title{manipulate plan basemaps}
\description{
  Set/get the basemap of a plan
}
\usage{
basem(object, ...)
"basem<-"(object, ..., value)
"basem<-.default"(object, ..., value)
basem.default(object, ...)
}
\arguments{
  \item{object}{plan}
  \item{\dots}{not used, for other generics}
  \item{value}{basemap}
}
\value{
	returns basemap
}
\note{ These methods are only for developers writing new score functions only. }

\author{ 
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah_altman/} 
 }


\examples{

  # read in a shapefile with demographic data
  data(suffolk.map)
  kplan<-createRandomPlan(suffolk.map,5)
  copy.of.suffolk.map<-basem(kplan)
  copy.of.suffolk.map==suffolk.map
  dim(copy.of.suffolk.map)
  
  # data frame w/out geography
  
  suffolk.data<-as.data.frame(suffolk.map)

}

\keyword{ IO }
\keyword{ spatial }
