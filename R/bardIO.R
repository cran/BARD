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
function(filen, id="BARDPlanID", gal=paste(filen,".GAL",sep=""), wantplan=FALSE,  projection=as.character(NA),queen=TRUE,...) {
  filen<-sub("\\.shp$","",filen,ignore.case=TRUE)
  #migrate to spatial data frames
  tmp.shape<-try( readShapePoly(filen,proj4string=CRS(projection),...) )
  
  if (inherits(tmp.shape,"try-error")) {
      return(NULL)
  }

  # Note: poly2nb is slow (even after my patches were integrated) so read from a file if present

    
  nb <- try(read.gal(gal))
  if (inherits(nb,"try-error")) {
        print(paste("No gal file found:",gal))
        nb<-NULL
  }
  
 if (length(grep("+proj=", projection, ignore.case = TRUE)) < 1) {
    	    print("Note: you have not specified a map projection. Please ensure that the projection you use provides reasonable representation of area, perimeter, and shape at district scale.")
 }
 
 if (wantplan) {
 	res<- spatialDataFrame2bardPlan(sdf=tmp.shape,nb=nb,id=id,queen=queen)
 } else {
 	res<- spatialDataFrame2bardBasemap(sdf=tmp.shape,nb=nb,queen=queen)
 }
 return (res)
}

bardPlan2spatialDataFrame<-function(x,id="BARDPlanID"){
	if (class(x)!="bardPlan") {
		warning("not a bard plan object") 
	}
	retval<-baseShape(basem(x))
	
	# this awkward construction is a workaround for the lack of a [<- operator  in the target object
	retval$TMPbardPlan2spatialDataFrame<-as.vector(x)
	names(retval)[which(names(retval)=="TMPbardPlan2spatialDataFrame")]<-id
	return(retval)
}

bardBasemap2spatialDataFrame<-function(x){
	if (class(x)!="bardBasemap") {
		warning("not a bard Basemap object") 
	}
	return(baseShape(x))
}

spatialDataFrame2bardPlan<-function(sdf,nb=NULL,queen=TRUE,id="BARDPlanID") {
	res<-sdf2bard(sdf=sdf,nb=nb,id=id,queen=queen)
	return(res$plan)
}

spatialDataFrame2bardBasemap<-function(sdf,nb=NULL,queen=TRUE,keepgeom=TRUE) {
	res<-sdf2bard(sdf=sdf,nb=nb,queen=queen,keepgeom=keepgeom)
	return(res)
}

sdf2bard<-function(sdf,nb,id,queen,keepgeom=TRUE) {
  buildIndex<-FALSE # until RGEOS widely available
  wantplan <- !missing(id)
  tmp.shape<-sdf
  tmp.nb<-nb
  
  if (class(sdf)!="SpatialPolygonsDataFrame") {
  	  warning("sdf is not a spatial polygons data frame")
  }
  
  
  #check projection and reproject if possible
  canTransform<-function() {
  	ow<-options(warn=-1)
	retval <- mrequire("rgdal",quietly=TRUE)
	options(ow)
	return(retval)
  }
  
  tmp.longlat<-FALSE
  if (length(grep("+proj=latlon",tmp.shape@proj4string@projargs,ignore.case=TRUE)>0) || length(grep("+proj=longlat",tmp.shape@proj4string@projargs,ignore.case=TRUE)>0)) {
  	tmp.longlat<-TRUE
  
  	if ( canTransform() ) {
  	  	  print ("Projecting map using albers equal area projection. This may take some time...")
  	  	  print(Sys.time())
  	 
  	  	  bb<-bbox(tmp.shape)
  	  	  newprojstring <- paste("+proj=aea", sep="") 
  	  	#newprojstring <- paste("+proj=aea ","+lat_1=",bb[1,1]," +lat_2=",bb[1,2]," +lat_0=",mean(bb[1,])," +lon_0=",mean(bb[2,])," +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs", sep="") 
  	  	tmp.shape2<-spTransform(tmp.shape, CRS(newprojstring))
  	  	if (inherits(tmp.shape2,"try-error")) {
  	  		warning("transformation failed")
      		} else {
      			tmp.shape<-tmp.shape2
			rm(tmp.shape2)
      			tmp.longlat<-FALSE

      		}
  	  }
  } 
 
 

  if (tmp.longlat) {
  	   warning("Using unprojected lat-lon coordinates. Area measures may be inaccurate. Consider installing (http://cran.r-project.org/web/packages/rgdal/) to have BARD re-project the map or transforming it with a separate program. The Albers Equal Areas (aea) projection with state-plane and state scale data in meter units is recommended.")
  }

  
  tmp.df <- tmp.shape@data
  
  
  mapid <- NULL 
  if ("ID" %in% names(tmp.df)) {
    mapid<-"ID"
  } else if ("id" %in% names(tmp.df)) {
    mapid<-"id"
  }
  
    # clean shape file
  tmp.cleaned<-NULL
  if (rgeostatus()) {
  	  tmp.shape<-RGEOSgeomCleaner(tmp.shape)
  	  if (is.null(attr(tmp.shape,"cleaned"))) {
  	  	  tmp.cleaned<-TRUE
  	  } else {
  	  	  tmp.cleaned<-attr(tmp.shape,"cleaned")
  	  }
  	  	  
  }
  
  # not an else, since top condition may change
  if (!is.null(tmp.nb)) {
  	  if (length(zeroWeights.nb(tmp.nb))>0) {
  	  	  warning("The neighborhood structure supplied is noncontigious,regenerating from the map")
  	  	   tmp.nb<-NULL
  	  }
}
  if (is.null(tmp.nb) && keepgeom) {
  	  
    estmin <- round((dim(tmp.shape)[1]*.002)^2 / 60,2)
    

    print(paste("Since no GAL file supplied, generating a neighborhood list, this may take a while  ... (estimated ",estmin," minutes)", sep=" "))
    print(paste("Starting", Sys.time(),sep=" "))
    tmp.nb<-myPoly2nb(tmp.shape,queen=queen)    
    print(paste("Ending", Sys.time(),sep=" "))


    
  }
  if (!keepgeom) {
	nb<-replicate(dim(tmp.df)[1],NULL)
  }

   retval<- list(myenv=new.env(),nb=tmp.nb,df=tmp.df,cleaned=tmp.cleaned)
   assign("self",retval,envir=retval$myenv)
   assign("shape",sdf,envir=retval$myenv)
  
  if (buildIndex) {
    estmin <- round((dim(tmp.shape)[1]*.05) / 60,2)
    print(Sys.time())
    print(paste("Building indices which will save a lot of time in perimeter calculations later ... ( estimated ",estmin," minutes)", sep=" "))
    print(paste("Starting", Sys.time(),sep=" "))

  }
  # generate shape info
  class(retval)<-"bardBasemap"

  tmp.info<-genBlockShapeInfo(retval,buildIndex=buildIndex)
  
  if (buildIndex) {
  	      print(paste("Ending", Sys.time(),sep=" "))
  }
  retval<-c(retval,tmp.info)
  class(retval)<-"bardBasemap"
  # workaround to memory issue in save with nexted environments...
  # also self has changed
  retval$myenv<-NULL
  retval$myenv<-new.env()
  assign("self",retval,envir=retval$myenv)
  if (keepgeom) {
  	assign("shape",sdf,envir=retval$myenv)
  }  else {
  	assign("shape",integer(0),envir=retval$myenv)
  }


  
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
  filen<-sub("\\.shp$","",filen,ignore.case=TRUE)
    basemap<-basem(plan)
  BARDplan <- as.vector(plan)
  tmpshape<-bardBasemap2spatialDataFrame(basemap)
  tmpshape[[id]]<-BARDplan
  
  saveres1<-try(writeSpatialShape(tmpshape,filen))
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

writeBardMap<-function(filen,basemap,verbatim=FALSE) {
    tmpshape<- get("shape",basemap$myenv)
    basemap$myenv<-NULL
    if (!verbatim) {
    	filen<-sub("_bard_save\\.Rdata$","",filen,ignore.case=TRUE)
    	filen<-paste(filen,"_bard_save.Rdata",sep="")
    }
    
    saveres <- try(save(file=filen, list=c("basemap","tmpshape")))
    retval<- !inherits(saveres,"try-error")
    return(invisible(retval))
}

readBardMap <-
function(filen,verbatim=FALSE) {
    if (!verbatim) {
    	filen<-sub("_bard_save\\.Rdata$","",filen,ignore.case=TRUE)
    	filen<-paste(filen,"_bard_save.Rdata",sep="")
    }
    loadval<-local({
      basemap<-NULL
      tmpshape<-NULL
      loadres<-try(load(filen))
      if (inherits(loadres,"try-error")) {
        lrval <- NULL
      } else {
      	    
	if (is.null(basemap) || is.null(tmpshape)) {
		# failed to load correct values 
		warning("failed to read image")
		lrval<-NULL
	} else {
		basemap$myenv<-new.env()
		assign("shape",tmpshape,basemap$myenv)
		assign("self",basemap,basemap$myenv)
        	lrval <- basemap
	}
      }
      lrval
    })
    basemap<-loadval
 
    basemap<-convertBaseMap.old(basemap)

    return(basemap) 
}


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
    filen<-sub("_bard_image\\.Rdata$","",filen,ignore.case=TRUE)
    filen<-paste(filen,"_bard_image.Rdata",sep="")
    loadval<-local({
      basemaps<-NULL
      plans<-NULL
      loadres<-try(load(filen))
      if (inherits(loadres,"try-error")) {
        lrval <- NULL
      } else {
      	    
	if (is.null(basemaps) && is.null(plans)) {
		# failed to load correct values 
		warning("failed to read image")
		lrval<-NULL
	} else {
        	lrval <- list(basemaps=basemaps,plans=plans)
	}
      }
      lrval
    })
    basemaps<-loadval$basemaps
    plans<-loadval$plans
    rm("loadval")
    if (inherits(basemaps,"bardBasemap")) {
		basemaps<-list(basemaps)
    }
    if (inherits(plans,"bardPlan")) {
		plans<-list(plans)
    }
		
    # check to convert from old formats
    if (!is.null(basemaps)) {
		basemaps<-sapply(basemaps,convertBaseMap.old,simplify=FALSE)
    }
    if (!is.null(plans)) {
		plans<-sapply(plans,
		function(x){ basem(x)<-convertBaseMap.old(basem(x)); x},
		simplify=FALSE)
    }
    retval <- list(basemaps=basemaps,plans=plans)

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
  filen<-sub("_bard_checkpoint\\.Rdata$","",filen, ignore.case=TRUE)
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
###  pre-calculate areas and perimeters
###

myUnionSpatialPolygons<-function(...) {
	
		if (rgeostatus()) {
			res<-gUnionCascaded(...)
		} else  {
			res<-unionSpatialPolygons(...)
		}
	return(res)		
}


genBlockShapeInfo<-function(basemap, buildIndex=TRUE) {
	
 bs <-bardBasemap2spatialDataFrame(basemap)
 bp <-bs@polygons
 nb <- dim(bs)[1]
 
 if (length(grep("+proj=latlon",bs@proj4string@projargs,ignore.case=TRUE)>0) || length(grep("+proj=longlat",bs@proj4string@projargs,ignore.case=TRUE)>0))  { tmp.longlat<-TRUE} else {tmp.longlat<-FALSE}
 
 
 if (buildIndex) {
   tmp.perims<-sapply(1:nb, function(x)genPerim(bp[x],longlat=tmp.longlat))
 }
   
   
   tmp.areas<-sapply(1:nb, function(x)bp[[x]]@area)
   
   tmp.bboxs<-sapply(1:nb,function(x)bbox(bp[[x]]))
  tmp.centroids<-sapply(1:nb, function(x)bp[[x]]@labpt)
  tmp.perims<-NULL
  # HMM -- performance of this is 1000x slower ?
  #tmp.bboxs<-sapply(1:dim(tmp.shape)[1],function(x)bbox(tmp.shape[x,]))
  #tmp.centroids<-sapply(1:dim(tmp.shape)[1],function(x)coordinates(tmp.shape[x,]))
  

  
     basemap$longlat<-tmp.longlat
  if (buildIndex) {
   # calculate overlapping perim between each connected block
     basemap$perims<-tmp.perims
     sharelist<-genPerimIndex(basemap)
  } else {
    sharelist<-NULL
  }
     tmp.timestamp<-Sys.time()

   return(list(perims=tmp.perims,sharedPerims=sharelist, areas=tmp.areas, bboxs=tmp.bboxs, centroids=tmp.centroids, timestamp=tmp.timestamp, longlat=tmp.longlat))
}


# calculate individual block perims
genPerim<-function(poly,longlat=FALSE) {
	
	sum(sapply(poly, 
   function(x)sapply(x@Polygons,function(x)LineLength(x@coords,longlat=longlat))))
}
  
genPerimIndex<-function(basemap) {

   sharelist <- unclass(basemap$nb);   attributes(sharelist)<-NULL
   tmp.longlat<-basemap$longlat
   tmp.perims<-basemap$perims
   
   for ( i in seq(1,length.out=dim(basemap)[1]) ) {
   	   tmplist<-sharelist[[i]]
   	   if (all(tmplist==0)) {
   	   	   tmplist<-integer()
   	   }
   	   for (j in seq(1,length.out=length(tmplist))) {
   	   	 tn <- tmplist[j]
   	   	 if (tn>=i) {
   	   	 	usub <- myUnionSpatialPolygons(bardBasemap2spatialDataFrame(basemap)[c(i,tn),1], c(1,1))
   	   	 	totalperim <- genPerim(usub@polygons,longlat=tmp.longlat)
   	   	 	sharedperim <-  tmp.perims[i] + tmp.perims[tn] - totalperim 
   	   	 	tmplist[j]<-sharedperim
   	   	 } else {
   	   	 	 tmplist[j] <- sharelist[[tn]][which(basemap$nb[[tn]] %in% i)] 
   	   	 }
   	   }
   	   sharelist[[i]]<-tmplist
   }	

   return(sharelist)
}


###
###  Functions to deal with the conversion of basemaps using the old 
#### polylist structure
###


isOldBasemap<-function(basemap) {
	if (class(basemap)!="bardBasemap") {
		return(NA)
	}
	if (is.null(basemap$shape)) {
		return(FALSE)
	}
	return (TRUE)
}

exportBardShape.polylist <-
function(plan,filen,id="BARDPlanID",gal=paste(filen,".GAL",sep="")) {
  filen<-sub("\\.shp$","",filen,ignore.case=TRUE)
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
	print("Attempting to convert basemap from previous polyList representation.")
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
convertBaseMap.old<-function(basemap) {
	if (inherits(basemap$shape,"SpatialPolygonsDataFrame")) {
		print("Map in old format, compacting memory")
		basemap$polys<-NULL
		basemap$myenv<-new.env()
		myenv<-basemap$myenv
		assign("self",basemap,envir=myenv)
		assign("shape",basemap$shape,envir=myenv)
		basemap$shape<-NULL
	} else {
		basemap<-convertBaseMap.polylist(basemap)
	
		if(is.null(basemap$longlat)) {
			print("Map built in a previous version, adding indices, please wait...")
			tmpinfo<-genBlockShapeInfo(basemap,buildIndex=TRUE)
			basemap<-c(basemap,tmpinfo)
			class(basemap)<-"bardBasemap"
		}
	}
	return(basemap)
}





###
###  Generic methods
###

print.bardBasemap<-function(x,...) {
	internal.print.bardBasemap(x,...)
}

HTML.bardBasemap<-function(x,...) {
	internal.print.bardBasemap(x,...,useHTML=TRUE)
}

internal.print.bardBasemap<-function(x,...,useHTML=FALSE) {
   if (useHTML) {
	htmlArgs<-list(...)
	print<-function(...)hprint(...,htmlArgs=htmlArgs)
	cat<-function(...)hcat(...,htmlArgs=htmlArgs)
	plot<-function(...)hplot(...,htmlArgs=htmlArgs)
  } else {
	htmlArgs<-NULL

  }
    cat("Bard Basemap\n\n")
    cat("Number of polys", dim(x)[1],"\n\n")
    cat("Variables:\n")
    print( names(x$df),...)
}

summary.bardBasemap<-function(object,...) {
  retval<-list()
  retval$npolys<-dim(object)[1]
  retval$dfsummary<-summary(object$df)
  class(retval)<-"bardBasemap.summary"
  return(retval)
}

print.bardBasemap.summary<-function(x,...) {
	internal.print.bardBasemap.summary(x,...)
}

HTML.bardBasemap.summary<-function(x,...) {
	internal.print.bardBasemap.summary(x,...,useHTML=TRUE)
}

internal.print.bardBasemap.summary<-function(x,...,useHTML=FALSE) {
   if (useHTML) {
	htmlArgs<-list(...)
	print<-function(...)hprint(...,htmlArgs=htmlArgs)
	cat<-function(...)hcat(...,htmlArgs=htmlArgs)
	plot<-function(...)hplot(...,htmlArgs=htmlArgs)
  } else {
	htmlArgs<-NULL

  }                                            
  print(x$dfsummary,...)
  cat("\n\n***\n\nNumber of polygons: ",x$npolys,"\n",...)
}


plot.bardBasemap<-function(x,...) {
 plot(bardBasemap2spatialDataFrame(x),...)
}

"==.bardBasemap"<-function(e1,e2) {
  retval<-identical(e1$myenv,e2$myenv)
  return(retval)
}

"!=.bardBasemap"<-function(e1,e2) {
  retval = !(e1==e2)
  return(retval)
}


as.data.frame.bardBasemap<-function (x, row.names = NULL, optional = FALSE, ...) {
	as.data.frame(bardBasemap2spatialDataFrame(x))
}

dim.bardBasemap<-function(x) {
	return(dim(bardBasemap2spatialDataFrame(x)))
}

RGEOSgeomCleaner<-function(x) {
	
	
	quietGisValid<-function(...) {
		ow<-options(warn=-1)
		retval<-gIsValid(...)
		options(ow)
		return(retval)
	}
		
	# note: only sets cleaned attribute if cleaning operation needed to be 
	# performed to prevent uneccessary duplication of x which can be large
	
	if (!BARD:::rgeostatus()) {
		return(x)
	}
	
	message("checking holes with rgeos")

	x<-createSPComment(x)
	invalid <- which(!quietGisValid(x,byid=T))
	
	if (length(invalid>0)) {
		warning("Found invalid polygons, attempting to clean")
		#attr(x,"cleaned")<-TRUE
		
		repaired<-x[invalid,0]
		
		invalid2<-which(!quietGisValid(repaired,byid=T))
		if (length(invalid2)>0) {
			message("buffering polys")
			repaired@polygons[invalid2]<-gBuffer(repaired[invalid2,0],byid=TRUE,width=0)@polygons
			invalid2<-which(!quietGisValid(repaired,byid=T))
		}
			
		if (length(invalid2)>0) {
			message("Filling poly holes")
			for (i in invalid2) {
				repaired@polygons[i]<-gUnionCascaded(repaired[i,0])@polygons
			}
			invalid2<-which(!quietGisValid(repaired,byid=T))
			if (length(invalid2)>0) {
				warning("Could not repair invalid polygons, run 	gIsValid() to diagnose")
				attr(x,"cleaned")<-FALSE
			} 
		}
		x@polygons[invalid]<-repaired@polygons

	}
	return(x)
}


