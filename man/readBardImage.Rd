\name{readBardImage}
\alias{readBardImage}
\alias{writeBardImage}
\title{Read and write BARD basemaps and plans as R images. }
\description{
  These are convenience functions to read and write BARD data as R images. This
  uses the native R image format, which is fast to read and to write. For a more
  portable, but slower approach see \code{importBardShape}
}
\usage{
readBardImage(filen)
writeBardImage(filen, basemaps=NULL, plans=NULL)
}

\arguments{
  \item{filen}{Name (and path to) file to be read or written}
  \item{basemaps}{List of BARD basemaps to be written}
  \item{plans}{List of BARD plans to be written}
}

\value{
  Read method returns:
  
  \item{plans}{list of plans}
  \item{basemaps}{list of basemaps}
  
  Write method returns logical success, invisibly.
}

\note{ \code{readBardImage} attempts to detect and convert images created by previous versions of BARD, however it may fail to convert them due to limits in the underlying R libraries it depends. Use \code{\link{exportBardShape}} in the previous version  \code{\link{importBardShape}} to migrate if automatic conversion is unsuccessful. }

\author{
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah_altman/} 
}

\seealso{ Other methods for \code{\link{readBardCheckpoint}},  \code{\link{writeBardCheckpoint}}, 
  \code{\link{importBardShape}}, \code{\link{exportBardShape}}  }

\examples{

  # read in a shapefile with demographic data
    suffolk.map <- importBardShape(system.file("shapefiles/suffolk_tracts.shp", package="BARD"))
    
  # choose number of districts
  ndists <- 5

  # create some initial plans
  kplan1 <- createKmeansPlan(suffolk.map,ndists)
  kplan2 <- createKmeansPlan(suffolk.map,ndists)

  # read and write images
  writeBardImage(paste(tempdir(),"/image1",sep=""),
    basemaps=list(suffolk.map),plans=list(kplan1,kplan2))
  tmp.image<-readBardImage(paste(tempdir(),"/image1",sep=""))

}

\keyword{ IO }
\keyword{ spatial }
