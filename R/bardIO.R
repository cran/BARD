##############################################################################
#
# bard I/O utility functions
#
# 
# This module contains methods to save and load basemaps and plan assignments
#
#
# A bardBasemap currently consists of:
#     - a shapefile structure as returned by read.shape() in MapTools
#        which contains the attribute data as $att.data
#     - a set of polys corresponding to the shape
#     - a neighborhood list
#
#
##############################################################################


##############################################################################
#
# importBardShape
#
# Reads shape file, generates polys and contiguity list (if not supplied in gal file)
#
# Arguments
#          filen - fully qualified name of .shp file
#          id - name of ID var in shape file
#          gal - fully qualified path to gal, if not included
#                 neighborhood list will be generated (but slowly)
#          wantplan
#
#
# Returns
#
#         bardBasemap    
#
##############################################################################

importBardShape <-
function(filen, id="BARDPlanID", gal=paste(filen,".GAL",sep=""), wantplan=FALSE) {
  filen<-sub("\\.shp","",filen)
  #migrate to spatial data frames
  tmp.shape<-try( readShapePoly(filen) )
  
  if (inherits(tmp.shape,"try-error")) {
      return(NULL)
  }
  tmp.polys <-tmp.shape@polygons

  tmp.df <- tmp.shape@data
  
  tmp.bboxs<-sapply(tmp.polys,function(x)bbox(x))
  tmp.centroids<-sapply(tmp.polys,function(x)x@labpt)
  # HMM -- performance of this is 1000x slower ?
  #tmp.bboxs<-sapply(1:dim(tmp.shape)[1],function(x)bbox(tmp.shape[x,]))
  #tmp.centroids<-sapply(1:dim(tmp.shape)[1],function(x)coordinates(tmp.shape[x,]))

  
  mapid <- NULL 
  if ("ID" %in% names(tmp.df)) {
    mapid<-"ID"
  } else if ("id" %in% names(tmp.df)) {
    mapid<-"id"
  }


  
  # Note: poly2nb is very slow, so read from a file if present

    
  tmp.nb <- try(read.gal(gal))
  if (inherits(tmp.nb,"try-error")) {
        print(paste("No gal file found:",gal))
        gal <- NULL
  }
  
  
  # not an else, since top condition may change
  if (is.null(gal)) {
    print("Generating gal file...")
    tmp.nb <- poly2nb(tmp.shape)
  }
  
  tmp.timestamp<-Sys.time()
  retval<-list(shape=tmp.shape,polys=tmp.polys,nb=tmp.nb,df=tmp.df,centroids=tmp.centroids, bboxs=tmp.bboxs, timestamp=tmp.timestamp)
  class(retval)<-"bardBasemap"
  if (wantplan) {
    tmp.plan <- NULL
    if (!is.null(tmp.df[[id]])) {
      tmp.plan <- tmp.df[[id]]
      attr(tmp.plan,"ndists")<-length(unique(tmp.plan))
      basem(tmp.plan)<-retval
      class(tmp.plan)<-"bardPlan"
    }
     retval<-list(retval,plan=tmp.plan)
  } else {
   
    return(retval)
  }
}




##############################################################################
#
# exportBardShape
#
# Exports plan as a shapefile, w
#
# Arguments
#          filen - fully qualified name of shapefile without extension,
#             [8 letters max]
#          basemap - bard basemap
#          plan - plan
#
# Returns
#          logical plan assignment
#
# Note
#
# The additional BARDplan variable indicates plan assignment
#
##############################################################################

exportBardShape <-
function(plan,filen,id="BARDPlanID",gal=paste(filen,".GAL",sep="")) {
  filen<-sub("\\.shp","",filen)
    basemap<-basem(plan)
  BARDplan <- as.vector(plan)
  basemap$shape@data[id]<-BARDplan
  
  saveres1<-try(writeSpatialShape(basemap$shape,filen))
  saveres2<-try(write.nb.gal(basemap$nb,gal))
  retval<-(!inherits(saveres1,"try-error")  &&  !inherits(saveres2,"try-error"))
  return(retval)
}


##############################################################################
#
# writeBardImage
# readBardImage
#
# Reads/writes bard objects as imagefiles
#
# Arguments
#          filen - fully qualified name of .image file without extensions
#          basemaps - bard basemap(s)
#          plans - plans)
#
#
# Returns
#
#         write - returns logical success
#         read - returns list of plans and maps
#
##############################################################################

writeBardImage <-
function(filen, basemaps=NULL, plans=NULL) {
    if (is.null(basemaps) && is.null(plans)) {
      warning("Saving empty image")
    }
    filen<-paste(filen,"_bard_image.Rdata",sep="")
    saveres <- try(save(file=filen, list=c("basemaps","plans")))
    retval<- !inherits(saveres,"try-error")
    return(invisible(retval))
}

readBardImage <-
function(filen) {
    filen<-paste(filen,"_bard_image.Rdata",sep="")
    retval<-local({
      basemaps<-NULL
      plans<-NULL
      loadres<-try(load(filen))
      if (inherits(loadres,"try-error")) {
        lrval <- NULL
      } else {
	if (is.null(basemaps) && is.null(plans)) {
		# failed to load correct values 
		lrval<-NULL
	} else {
		# check to convert from old formats
		if (!is.null(basemaps)) {
			basemaps<-sapply(basemaps,convertBaseMap.old,simplify=FALSE)
		}
		if (!is.null(plans)) {
			plans<-sapply(plans,
			function(x){ basem(x)<-convertBaseMap.old(basem(x)); x},
			simplify=FALSE)
		}
        	lrval <- list(basemaps=basemaps,plans=plans)
	}
      }
      lrval
    })
    return(retval)
}

##############################################################################
#
# writeBardCheckpoint
# readBardCheckpoint
#
# Reads/writes entire state of bard operation, and attempts to restart
# bard on load
#
# Arguments
#          filen - fully qualified name of .image file without extensions
#          restart.fun - what to run to restart Bard?
#
# Returns
#         write - returns logical success
#         read - returns logical success , if continue false
#                returns nothing and transfers control to .First if continue ==true
#
# Note
#        read.checkpoint used for its side effect -- loads .GlobalEnv with state
#           
#
##############################################################################

writeBardCheckpoint <-
function(filen, restart.fun=NULL ) {
  filen<-paste(filen,"_bard_checkpoint.Rdata",sep="")
  if (is.null(restart.fun)) {
    restart.fun<-function()cat("Welcome back to BARD\n")
  }
  .First <- function(...) {
    if(!require(BARD)) {
      cat("BARD module not found, trying to obtain it ...\n")
      install.packages("BARD",dependencies=TRUE)
    } 
    restart.fun()
  }
  assign(".First",.First,pos=1)
  on.exit(rm(".First",pos=1))
  saveres<-try(save(list=ls(pos=1,all.names=TRUE),file=filen))
  retval<- !inherits(saveres,"try-error")
  return(retval)
}

readBardCheckpoint <-
function(filen, continue=TRUE) {
  filen<-paste(filen,"_bard_checkpoint.Rdata",sep="")
  loadres<-try(load(filen, .GlobalEnv))
  retval <- !inherits(loadres,"try-error")
  # dummy function for R CMD check
  .First<-function(){}
  if (continue && retval ) {
    if(exists(".First",envir=.GlobalEnv)) {
      rm(".First")
      on.exit(.First())
    }
  }
  return(retval)
}


###
###  Functions to deal with the conversion of basemaps using the old 
#### polylist structure
###

importBardShape.polyList <-
function(filen, id="BARDPlanID", gal=paste(filen,".GAL",sep=""), wantplan=FALSE) {
  filen<-sub("\\.shp","",filen)
  #migrate to spatial data frames
  ow<-options(warn=-1)
  tmp.shape<-try( maptools:::read.shape(filen) )
  options(ow)
  if (inherits(tmp.shape,"try-error")) {
      return(NULL)
  }
  tmp.df <- tmp.shape$att.data
  mapid <- NULL 
  if ("ID" %in% names(tmp.df)) {
    mapid<-"ID"
  } else if ("id" %in% names(tmp.df)) {
    mapid<-"id"
  }
  #migrate to spatial data frames
  ow<-options(warn=-1)
  tmp.polys <-  maptools:::Map2poly(tmp.shape,region.id=TRUE )
  options(ow)
  
  # Note: poly2nb is very slow, so read from a file if present

    
  tmp.nb <- try(read.gal(gal))
  if (inherits(tmp.nb,"try-error")) {
        print(paste("No gal file found:",gal))
        gal <- NULL
  }
  
  
  # not an else, since top condition may change
  if (is.null(gal)) {
    print("Generating gal file...")
    tmp.nb <- poly2nb(tmp.polys)
  }
  
  tmp.timestamp<-Sys.time()
  retval<-list(shape=tmp.shape,polys=tmp.polys,nb=tmp.nb,df=tmp.df,timestamp=tmp.timestamp)
  class(retval)<-"bardBasemap"
  if (wantplan) {
    tmp.plan <- NULL
    if (!is.null(tmp.df[[id]])) {
      tmp.plan <- tmp.df[[id]]
      attr(tmp.plan,"ndists")<-length(unique(tmp.plan))
      basem(tmp.plan)<-retval
      class(tmp.plan)<-"bardPlan"
    }
     retval<-list(retval,plan=tmp.plan)
  } else {
   
    return(retval)
  }
}

importBardShape.old <- importBardShape.polyList 

isOldBasemap<-function(basemap) {
	if (class(basemap)!="bardBasemap") {
		return(NA)
	}
	if (inherits(basemap$shape,"SpatialPolygonsDataFrame")) {
		return(FALSE)
	}
	return (TRUE)
}

exportBardShape.polylist <-
function(plan,filen,id="BARDPlanID",gal=paste(filen,".GAL",sep="")) {
  filen<-sub("\\.shp","",filen)
    basemap<-basem(plan)
  BARDplan <- as.vector(plan)
  newDf <- cbind(basemap$df,BARDplan)
  names(newDf)[length(newDf)] <- id
  
  #migrate to spatial data frames
  ow<-options(warn=-1)
  saveres1<-try(maptools:::write.polylistShape(basemap$polys,newDf,filen))
  options(ow)
  saveres2<-try(write.nb.gal(basemap$nb,gal))
  retval<-(!inherits(saveres1,"try-error")  &&  !inherits(saveres2,"try-error"))
  return(retval)
}

exportBardShape.old <-
exportBardShape.polylist 

convertBaseMap.polylist<-function(basemap) {
	if (!isOldBasemap(basemap)) {
		return(basemap)
	}
	warning("Attempting to convert basemap from previous polyList representation.")
	fn.tmp <- tempfile()
	plan.tmp<- createAssignedPlan(basemap,replicate(length(basemap$polys),1))
	exportBardShape.old(plan.tmp, fn.tmp,id="TMPID")
	retval<-importBardShape(fn.tmp,id="TMPID")

	# remove tmp id
	tmpcol<-which(names(retval$shape@data)=="TMPID")
	retval$shape@data<-retval$shape@data[,-tmpcol]
	retval$df<-retval$df[,-tmpcol]

	return(retval)
}
convertBaseMap.old<-convertBaseMap.polylist		

###
###  Generic methods
###

print.bardBasemap<-function(x,...) {
    cat("Bard Basemap\n\n")
    cat("Number of polys", length(x$polys),"\n\n")
    cat("Variables:\n")
    print( names(x$df),...)
}

summary.bardBasemap<-function(object,...) {
  retval<-list()
  retval$npolys<-length(object$polys)
  retval$dfsummary<-summary(object$df)
  class(retval)<-"bardBasemap.summary"
  return(retval)
}

print.bardBasemap.summary<-function(x,...) {
  print(x$dfsummary,...)
  cat("\n\n***\n\nNumber of polygons: ",x$npolys,"\n",...)
}

plot.bardBasemap<-function(x,...) {
 plot(x$shape,...)
}

"==.bardBasemap"<-function(e1,e2) {
  ep1<-attributes(e1$shape$Shape)
  ep2<-attributes(e2$shape$Shape)
  retval<-identical(ep1,ep2)
  return(retval)
}

"!=.bardBasemap"<-function(e1,e2) {
  retval = !(e1==e2)
  return(retval)
}



  
