\name{spatialDataFrame2bardBasemap}
\alias{spatialDataFrame2bardBasemap}
\alias{spatialDataFrame2bardPlan}
\alias{bardPlan2spatialDataFrame}
\alias{bardBasemap2spatialDataFrame}
\title{conversion methods for BARD objects}
\description{
	converts BARD basemap to SpatialPolygonsDataFrame and back
}
\usage{
spatialDataFrame2bardBasemap(sdf,nb=NULL,queen=TRUE,keepgeom=TRUE) 
spatialDataFrame2bardPlan(sdf,nb=NULL,queen=TRUE,id="BARDPlanID") 
bardBasemap2spatialDataFrame(x)
bardPlan2spatialDataFrame(x,id="BARDPlanID")
}

\arguments{
  \item{sdf}{spatial data frame object}
  \item{x}{bardBasemap object}
  \item{nb}{object of class \code{nb} }
  \item{queen}{use queen contiguity when running \code{poly2nb} }
  \item{id}{name of column used to identify plan assingnment}
  \item{keepgeom}{keep geometry}
}
\value{
	returns basemap,plan, or spatialPolygonsDataFrame, respectively
}

\note{ 
	Conversion from Spatial Data Frame's to bard objects is potentially slow, because BARD objects use index structures not available in the sdf's, and the conversion must create these indices.

   keepgeom=FALSE is only for use with a limited set of scoring functions, does not support plots or
   district generation. Avoid if you're not absolutely sure.
}
\author{ 
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah_altman/} 
 }


\examples{
 xx <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], IDvar="FIPSNO", proj4string=CRS("+proj=longlat +ellps=clrk66"))
 data(suffolk.map)
 suffolk.sdf <- bardBasemap2spatialDataFrame(suffolk.map)
 xx.map<-spatialDataFrame2bardBasemap(xx,queen=TRUE)
 data(iacounty.sdf)
 iacounty.map<-spatialDataFrame2bardBasemap(iacounty.sdf,queen=FALSE)
 rpplan<-createRandomPopPlan(iacounty.map,10) 
 iacounty2.sdf <-bardPlan2spatialDataFrame(rpplan)
 rpplan2<-spatialDataFrame2bardPlan(iacounty2.sdf)
 \dontshow{
	if (class(suffolk.sdf)!="SpatialPolygonsDataFrame"){
		warning("conversion from bardBasemap failed")
	}
	if (class("xx.map") !="bardBasemap") {
		warning("conversion from spatial data frame failed")
	}
 }
}

\keyword{ IO }
\keyword{ spatial }
