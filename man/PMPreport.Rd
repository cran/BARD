\name{PMPreport}
\alias{PMPreport}
\title{generate a specialized summary report on set of plans, for the Public Mapping Project}
\description{
	This generates a districting report on district charcteristics. It is is used in the Public Mapping Project system.
}


\usage{
	PMPreport(bardMap,
	blockAssignmentID="BARDPlanID",
	popVar=NULL,
	popVarExtra=NULL,
	ratioVars=NULL,
	splitVars = NULL,
	blockLabelVar=NULL,
	repCompactness=TRUE,
	repCompactnessExtra=FALSE,
	repSpatial=TRUE,
	repSpatialExtra=FALSE,
	useHTML=TRUE,
	districtPolys=NULL,
	...)
}




\arguments{
\item{bardMap}{a bard basemap or plan.}
\item{blockAssignmentID}{Not needed if bardMap is a plan. If bardMap is a basemap, name of variable in the basemap containing plan id's or a vector of ID assignments, as per \code{as.numeric(plan)} on any bard plan}
\item{popVar}{List containing single population variable, and a tolerance variable for equal population. This will generate a population equality report . Use NULL to suppress this report. }
\item{popVarExtra}{list of multiple additional demographic variables. The levels and district homogeneity of these will be reported. Use NULL to suppress this report}
\item{ratioVars}{A nested list of lists. Each sub list contains a demonitator, numerator and threshold. The resulting report will show which districts are cumulatively above the threshold for each variable}
\item{splitVars}{A list of variables in the basemap. These variables should indicate for each unit in the basemap the ID of some higher level geography to which the unit belongs (e.g. a county) The resulting report will show the number of times each district splits the higher-level geography.}
\item{blockLabelVar}{The name of each underlying unit}
\item{repCompactness}{Flag. Whether to include a report on compactness.}
\item{repCompactnessExtra}{Flag. Include more compactness scores in the compactness report.}
\item{repSpatial}{Flag. Report on unassigned blocks.}
\item{repSpatialExtra}{Flag. Also report on contiguity, other spatial features.}
\item{useHTML}{use html formatting for reports}
\item{districtPolys}{aggregated polygons, one for each district, used for optimization}
\item{\ldots}{arguments passed on to \code{print} and \code{HTML}}
}

\details{
 This produces a detailed report. See the examples. 
}

\value{
  Nothing. The function is used for printing and plotting effect.... 
}

\author{ 
  Micah Altman
  \email{Micah\_Altman@harvard.edu}
  \url{http://www.hmdc.harvard.edu/micah\_altman/} 
}

\references{
	\url{http://publicmapping.org}
}

\seealso{ 
  Scoring functions: \code{\link{calcContiguityScore}}
  Other reports comparing multiple plans:\code{reportPlans}
}

 
\examples{
  data(suffolk.map)
    
  # choose number of districts
  ndists <- 5

  # create some initial plans
  kplan <- createKmeansPlan(suffolk.map,ndists)
  is.na(kplan[c(1,20,200)])<-TRUE
  
  # name the districts for the report...
  levels(kplan)<-c("A","Beta","3+","Fred's District","Ceci n'est pas une district")

  gpclibPermit()
  aggregatePolys<-unionSpatialPolygons(bardBasemap2spatialDataFrame(suffolk.map),kplan)

PMPreport(kplan, popVar=list("Total Population"="POP",tolerance=.01), popVarExtra=list("Families"="FAMILIES","Vacant Houses"="VACANT"),ratioVars=list(
		"Majority Minority Districts"=list(
			denominator=list("Total Population"="POP"),
			threshold=.6,
			numerators=list("Black Population"="BLACK", "Hispanic Population"="HISPANIC")
		   )
        ), splitVars = list("Tract"="TRT2000","County"="FIPSSTCO"), blockLabelVar="ID",
	repCompactness=TRUE,repCompactnessExtra=TRUE,
	repSpatial=TRUE,	repSpatialExtra=TRUE,
	useHTML=FALSE,	districtPolys=aggregatePolys)
	
#alternately -- can be a factor or integer list -- gaps don't matter
fplan<-factor(2*kplan,labels=levels(kplan))
PMPreport(suffolk.map, blockAssignmentID=fplan, popVar=list("Total Population"="POP",tolerance=.01), popVarExtra=list("Families"="FAMILIES","Vacant Houses"="VACANT"),ratioVars=list(
		"Majority Minority Districts"=list(
			denominator=list("Total Population"="POP"),
			threshold=.6,
			numerators=list("Black Population"="BLACK", "Hispanic Population"="HISPANIC")
		   )
        ), splitVars = list("Tract"="TRT2000","County"="FIPSSTCO"), blockLabelVar="ID",
	repCompactness=TRUE,repCompactnessExtra=TRUE,
	repSpatial=TRUE,	repSpatialExtra=TRUE,
	useHTML=FALSE,	districtPolys=aggregatePolys)

}
\keyword{IO}
\keyword{spatial}
\keyword{optimize}
\keyword{models}
\keyword{distribution}
