\name{importBlockEquiv}
\alias{blockEquiv2bardPlan}
\alias{importBlockEquiv}
\alias{exportBlockEquiv}
\title{Read  and write block equivalency files }
\description{
  These are convenience functions to read block equivalency files.
}
\usage{
importBlockEquiv(filen,basemap,idvar="GEOID10")
exportBlockEquiv(filen,plan,idvar="GEOID10")
blockEquiv2bardPlan(bedf,basemap,idvar="GEOID10") 
}

\arguments{
  \item{filen}{Name (and path to) file to be read or written}
  \item{basemap}{single bard map }
  \item{bedf}{block equivalency data frame}
  \item{plan}{plan to write}
  \item{idvar}{name of variable containing block ids in bardMap}
}

\value{
  Returns: bard plan
}

\note{  
	A block equivalency file is a comma-separated values file with no column/row names, no quotes or
	comments, and either two or three commons.
	
	The first column indicates the geographic identifier at the block level.
	The second column indicates the numeric district identifier 
	The third (optional) column indicates the number of seats in (magnitude of) the district
	
	For other file formats, use \code{read.table}, construct a 2 or 3 column data frame, and
	then use \code{blockEquiv2bardPlan}

}
\author{
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah_altman/} 
}

\seealso{ Other methods for \code{\link{readBardCheckpoint}},  \code{\link{writeBardCheckpoint}}, 
  \code{\link{importBardShape}}, \code{\link{exportBardShape}}  }

\examples{

  # read in a shapefile with demographic data
  data(suffolk.map)    
  kplan<-createKmeansPlan(suffolk.map,5)

  # read and write images
  exportBlockEquiv(paste(tempdir(),"/be.csv",sep=""), kplan,idvar="TRACTID")
  kplan2<-importBlockEquiv(paste(tempdir(),"/be.csv",sep=""),suffolk.map,idvar="TRACTID")
  kplan3<-blockEquiv2bardPlan(data.frame(as.data.frame(suffolk.map)["TRACTID"],as.vector(kplan)),suffolk.map,idvar="TRACTID")
 
}

\keyword{ IO }
\keyword{ spatial }
