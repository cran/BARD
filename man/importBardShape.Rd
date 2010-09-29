\name{importBardShape}
\alias{importBardShape}
\alias{exportBardShape}
\title{Import and export bard plans and basemaps as shapefiles }
\description{
These are convenience functions to read and write BARD data as ESRI shapefiles. 
For a faster method to store these in native form see \code{readBardImage}

}
\usage{
exportBardShape(plan, filen, id = "BARDPlanID", gal = paste(filen, ".GAL", sep = ""))
importBardShape(filen, id="BARDPlanID", gal=paste(filen,".GAL",sep=""), wantplan=FALSE, projection =
                 as.character(NA),queen=TRUE,...) 
}

\arguments{
 \item{filen}{Name (and path to) file to be read or written}
  \item{id}{name of polygon block (NOT plan) id variable}
  \item{plan}{Plan assignment}
  \item{wantplan}{whether to extract embedded BARD plan from shapefile}
  \item{projection}{map projection}
  \item{gal}{GAL style contiguity list. Contiguity list will be regenerated if not supplied, but this is slow.}
  \item{queen}{Whether to use queen contiguity in poly2nb}
  \item{\ldots}{additional arguments to pass to readShapePoly}
}


\value{

  Read method returns a basemap by default.
  If "wantplan" is TRUE, read method returns a list with:

  \item{plan}{a plan, as identified by the BARDplan variable in the shapefile}
  \item{basemap}{the basemap}
  
    Write method returns logical success, invisibly.
}

\note{
  - Using \code{createAssignedPlan} with an imported plan is generally more memory efficient than using \code{wantplan=TRUE}.
  

  - Creating a contiguity list has been dramatically sped up ,however it can be somewhat slow for very large files (> 100000 units) unless \code{rgeo} is installed. Using GEODA or another program to generate GAL files is much faster for huge maps.
  
  - A reasonable map projection should be used. If the map is unprojected,  area calculations are potentially inaccurate. If rgdal is available BARD will attempt to reproject an unprojected map for increased accuracy.
}

\author{ 
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah_altman/} 
 }


\seealso{ 
Other methods for \code{\link{readBardCheckpoint}},  \code{\link{writeBardCheckpoint}}, 
\code{\link{readBardImage}}, \code{\link{writeBardImage}},
  \code{\link{readBardMap}}, \code{\link{writeBardMap} }
 }
 
\examples{

  # read in a shapefile with demographic data
  suffolk.map <- importBardShape(file.path(system.file("shapefiles", package="BARD"),"suffolk_tracts"))
    
  # choose number of districts
  ndists <- 5

  # create some initial plans
  kplan1 <- createKmeansPlan(suffolk.map,ndists)
  
  # read and write images
  exportBardShape(file.path(tempdir(),"shape1"),plan=kplan1)

  # reimport 
  suffolk.map2<-importBardShape(file.path(tempdir(),"shape1"))
  kplan2<-createAssignedPlan(suffolk.map2)

}

\keyword{ IO }
\keyword{ spatial }
