\name{readBardMap}
\alias{readBardMap}
\alias{writeBardMap}
\title{Read and write single BARD basemaps fast. }
\description{
  These are convenience functions to read and write BARD data as R images. This
  uses the native R image format, which is fast to read and to write. Specialized
  techniques avoid memory duplication.
}
\usage{
readBardMap(filen,verbatim=FALSE)
writeBardMap(filen, basemap,verbatim=FALSE)
}

\arguments{
  \item{filen}{Name (and path to) file to be read or written}
  \item{basemap}{single bard map }
  \item{verbatim}{if false appends _bard_nosave.Rdata to filename *if* filename does not already contain this}
}

\value{
  Read method returns:
  
  \item{basemap}{BARD basemaps}
  
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
    

  # read and write images
  writeBardMap(paste(tempdir(),"/image1",sep=""), basemap=suffolk.map)
  tmp.image<-readBardMap(paste(tempdir(),"/image1",sep=""))

}

\keyword{ IO }
\keyword{ spatial }
