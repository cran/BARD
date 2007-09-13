\name{readBardCheckpoint}
\alias{readBardCheckpoint}
\alias{writeBardCheckpoint}
\title{ Write checkpoint of BARD state, read checkpoint and restart. }
\description{
  This checkpoints the BARD program state as an R image, and can be
  used to restart BARD.
}
\usage{
  readBardCheckpoint(filen, continue = TRUE)
  writeBardCheckpoint(filen, restart.fun = NULL)
}

\arguments{
  \item{filen}{Name (and path to) file to be read or written}
  \item{continue}{Run restart function}
  \item{restart.fun}{Function to run after reading back checkpoint to restart}
}

\details{
  These functions read and write parts of the .GlobalEnv.  
}

\value{
  Write return logical success invisibly. 
  Read returns logical success invisibly, and restores selected .GlobalEnv state. 
  If doContinue is TRUE, and read is successful, it launches restart function on exit.
}

\author{ 
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah_altman/} 
}

\seealso{Other methods for \code{\link{readBardImage}}, \code{\link{writeBardImage}},
   \code{\link{exportBardShape}}, \code{\link{importBardShape}}
}

\examples{
  # read in a shapefile with demographic data
  suffolk.map <- importBardShape(
    system.file("shapefiles/suffolk_tracts.shp", package="BARD"))
    
  # choose number of districts
  ndists <- 5

  # create some initial plans
  kplan1 <- createKmeansPlan(suffolk.map,ndists)
  kplan2 <- createKmeansPlan(suffolk.map,ndists)

  # read and write images
  writeBardCheckpoint(paste(tempdir(),"/checkpoint1",sep=""),
    restart.fun=function(){cat("Welcome back\n")})
  readBardCheckpoint(paste(tempdir(),"/checkpoint1",sep=""))
}

\keyword{ spatial }
\keyword{ IO }
