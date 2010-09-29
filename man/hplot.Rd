\name{hplot}
\alias{hplot}
\alias{copyR2HTMLfiles}
\title{HTML methods for BARD objects}
\description{
        BARD provides various functions to make it easy to produce HTML output with R2HTML
}
\usage{
hplot(x,y, ... , htmlFile=NULL,  graphfileBase=NULL, bitmapArgs=NULL,htmlArgs=NULL)
copyR2HTMLfiles(outDir)
}
\arguments{
  \item{x}{ object to plotted }
  \item{y}{ optional object to plotted  }
  \item{\dots}{ additional parameters passed to plot.default }
  \item{htmlFile}{ location of html file to write to, will default to contents of .HTML.file or tempdir/index.html }
  \item{graphfileBase}{ base name for graph files , defaults to "graph"}
  \item{bitmapArgs}{arguments to pass to bitmap}
  \item{htmlArgs}{arguments to pass to HTMLInsertFile}
  \item{outDir}{target directory}
}
\note{
        \code{hplot} can be used instead of \code{plot} to easily insert plots into the HTML output. it calls \code{plot()} to plot the object, bitmap() to output it as bitmaps, and HTMLInsertGraph to generate HTML referring to it.

	\code{copyR2HTMLfiles} is used to copy the css files and other support files needed by R2HTML into a temporay directory

  	\code{HTML} methods for BARD objects pretty prints those in HTML.
}
\value{
        None
}
\examples{
 data(suffolk.map)
  numberdists <- 5
  kplan <- createKmeansPlan(suffolk.map,numberdists)
  rplan <- createRandomPlan(suffolk.map,numberdists)
  if (require("R2HTML",quietly=TRUE)) {
	targetDIR<- file.path(tempdir(),"R2HTML")
	dir.create(targetDIR)
	copyR2HTMLfiles(targetDIR)
	target <- HTMLInitFile(targetDIR,filename="sample", BackGroundColor="#BBBBEE", Title="BARD Web Output")
	HTML.title("BARD on the web", HR=2,file=target)
	HTML("<p>Example of web output using BARD</p>",file=target)
	hplot(kplan)
	HTML("<p>Plan details</p>",file=target)
	HTML(kplan,file=target)
	HTMLEndFile()
	\dontrun{
	browseURL(paste("file://",target,sep=""))
	}
    }
}

\author{
Micah Altman
\email{micah_altman\@harvard.edu}
}


\seealso{\code{\link{reportPlans}}, \code{\link[R2HTML]{HTMLInsertGraph}}, \code{\link{bitmap}}, \code{\link{plot}} }

\keyword{iplot}
\keyword{IO}
\keyword{print}

