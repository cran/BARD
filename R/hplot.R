#
# hplot
#
# Wrapper functions for calling R2HTML cleanly -- substitute functions
# for cat, print, plot
# 

# if HTML not available...
HTML<-try(R2HTML::HTML,silent=TRUE)
if (inherits(HTML,"try-error")){
        HTML<-print
}


printTitle <- function(x,HR=2,useHTML=FALSE,htmlArgs=NULL){
	if (useHTML) {
		do.call("HTML.title",c(list(x=x,HR=HR,CSSclass="BARDtitle",append=TRUE),htmlArgs))

	} else {
		 cat(paste("\n***\n",x,"\n***\n",sep=""))
	}
}

# HTML PLOTTING 
hprint<-function(x,..., htmlArgs=NULL) {
	do.call("HTML",args=c(x=list(x),list(...),htmlArgs))
}


hcat<- function(..., file = "", sep = " ", fill = FALSE, labels = NULL, 
append = FALSE, htmlArgs=NULL) {
	if ((fill || !is.null(labels) || append || file != "")) {
		warning("Unsupported option ignored.")
	}
	output <- paste(..., sep=sep)
	do.call("HTML",c(list(x=output),htmlArgs))
	
}
	

hplot <-   function(x,y, ... , htmlFile=NULL,  graphfileBase=NULL, bitmapArgs=NULL,htmlArgs=NULL) {
  UseMethod("hplot")
}

hplot.default<-  function(x,y, ... , htmlFile=NULL,  graphfileBase=NULL, bitmapArgs=NULL, htmlArgs=NULL , ext=NULL) {
  # set defaults for args
  op<-par(no.readonly=TRUE)
  par(ask=FALSE)
  tmp = hplot.default.args(htmlFile,graphfileBase,bitmapArgs,htmlArgs)
  htmlFile=tmp$htmlFile; graphDir=tmp$graphDir; graphfileBase=tmp$graphfileBase; 
  bitmapArgs=tmp$bitmapArgs; htmlArgs=tmp$htmlArgs; ext=tmp$ext
  
  # make base unique
  graphfileBase = paste( graphfileBase,  
    as.character(as.numeric(Sys.time())),
        as.integer(runif(1,min=0,max=.Machine$integer.max)), sep="A" )

  bitmapArgs$file=file.path(graphDir,paste(graphfileBase, "_%03d" ,".",ext, sep=""))
  if (class(try(do.call( "bitmap", bitmapArgs ),silent=T))=="try-error") { 
    # bitmap failed, probably missing ghostscript
    pngArgs=list()
    pngArgs$filename=bitmapArgs$file # note different syntax :-(
    pngArgs$res=bitmapArgs$res
    pngArgs$width=bitmapArgs$width
    pngArgs$height=bitmapArgs$height
    pngArgs$pointsize=bitmapArgs$pointsize
    do.call("png",pngArgs)
   }
  if (missing(y)) {
      plot(x,...)
  } else {
      plot(x,y,...)
  }
  dev.off()
  for (fn in list.files(graphDir,pattern=graphfileBase)) {
    htmlArgs$GraphFileName=fn
    htmlArgs$file=htmlFile
    do.call( "HTMLInsertGraph", htmlArgs ) 
    cat("</p>\n",sep="",append=TRUE,file=htmlFile)
    
  }
  par(op)
  invisible()
}

hplot.default.args<-function (htmlFile=NULL,
    graphfileBase=NULL,bitmapArgs=NULL,htmlArgs=NULL) {

  if (is.null(htmlFile)) {
     if ( exists(".HTML.file", envir = .GlobalEnv)) {
        htmlFile= get(".HTML.file", envir = .GlobalEnv)
     } else {
        htmlFile = file.path(tempdir(),"index.html");
     }
  }

  graphDir=dirname(htmlFile)

  if (is.null(graphfileBase)) {
     graphfileBase= paste("graph", Sys.getpid(), sep="")
  }

  if (is.null(bitmapArgs)) {
    bitmapArgs=list()
  }
  if (is.null(bitmapArgs$type)) {
    bitmapArgs$type="png256"
  }
  if ( bitmapArgs$type!="png256"  && bitmapArgs$type!="png16m" && bitmapArgs$type != "jpeg"){
    bitmapArgs$type="png256"
  }
  if (bitmapArgs$type=="jpeg") {
    ext="jpeg"
  } else {
    ext="png"
  }
  if (is.null(bitmapArgs$res)) {
    bitmapArgs$res=150;
  }
  if (is.null(bitmapArgs$width)) {
    bitmapArgs$width=4;
  }
  if (is.null(bitmapArgs$height)) {
    bitmapArgs$height=4;
  }

  if (is.null(htmlArgs)) {
    htmlArgs=list()
  } 
  htmlArgs$append=TRUE
  htmlArgs$file=htmlFile
  if (is.null(htmlArgs$WidthHTML)) {
    htmlArgs$WidthHTML = bitmapArgs$width * bitmapArgs$res
  }
  if (is.null(htmlArgs$HeightHTML)) {
    htmlArgs$HeightHTML = bitmapArgs$height * bitmapArgs$res
  }
  
  return(list("graphDir"=graphDir, "graphfileBase"=graphfileBase, "bitmapArgs"=bitmapArgs,
    "htmlFile"=htmlFile, "htmlArgs"=htmlArgs, "ext"=ext))
}
  

plot.grob<-function(x,...){
     grid.draw(x)
}

#
# Copy files necessary for HTML formatting to target directory 
#

copyR2HTMLfiles<-function(outDir){
  assign("HTMLenv",new.env(parent=.GlobalEnv),envir=.GlobalEnv)
  assign(".HTML.outdir",outDir,envir=get("HTMLenv",envir=.GlobalEnv))
  r2hbasedir=   file.path(.find.package(package = "R2HTML"),"output")
  file.copy(file.path(r2hbasedir,"R2HTML.css"),outDir)
  file.copy(file.path(r2hbasedir,"ASCIIMathML.js"),outDir)
  file.copy(file.path(r2hbasedir,"gridR2HTML.css"),outDir)
  file.copy(file.path(r2hbasedir,"gridR2HTML.js"),outDir)
  dir.create(file.path(outDir,"runtime/styles/xp/"),recursive=T,showWarnings=F)
  file.copy(file.path(r2hbasedir,"grid.css"),file.path(outDir,"runtime","styles","xp/"))
  dir.create(file.path(outDir,"runtime","lib"),recursive=T,showWarnings=F)
  file.copy(file.path(r2hbasedir,"grid.js"),file.path(outDir,"runtime","lib"))
}

