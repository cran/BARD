\name{suffolk.map}
\alias{suffolk.map}
\docType{data}
\title{
suffolk map 
}
\description{
tract level map data
}

\usage{data(suffolk.map)}
\format{
  The format is:
List of 12
 $ shape       :Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
 $ nb          :List 
 $ df          :'data.frame'
 $ perims      : num 
 $ sharedPerims:List 
 $ areas       : num 
 $ bboxs       : num 
 $ perims      : num 
 $ centroids   : num 
 $ timestamp   : POSIXct[1:1]
 $ longlat     : logi 
 - attr(*, "class")= chr "bardBasemap"
}
\details{
Map of census tracts in Suffolk county, new york supplemented with 2000 demographic data.
}
\examples{
data(suffolk.map)
}
\keyword{datasets}
