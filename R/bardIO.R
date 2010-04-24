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
function(filen, id="BARDPlanID", gal=paste(filen,".GAL",sep=""), wantplan=FALSE,  projection=as.character(NA),...) {
  buildIndex<-TRUE
  filen<-sub("\\.shp$","",filen,ignore.case=TRUE)
  #migrate to spatial data frames
  tmp.shape<-try( readShapePoly(filen,proj4string=CRS(projection),...) )
  
  if (inherits(tmp.shape,"try-error")) {
      return(NULL)
  }

  #check projection and reproject if possible
  canTransform<-function() {
	mrequire<-require
	retval <- mrequire("rgdal",quietly=TRUE)
	return(retval)
  }
  
 if (length(grep("+proj=", projection, ignore.case = TRUE)) < 1) {
    	    print("Note: you have not specified a map projection. Please ensure that the projection you use provides reasonable representation of area, perimeter, and shape at district scale.")
  }

  tmp.longlat<-FALSE
  if (length(grep("+proj=latlon",tmp.shape@proj4string@projargs,ignore.case=TRUE)>0) || length(grep("+proj=longlat",tmp.shape@proj4string@projargs,ignore.case=TRUE)>0)) {
  	  if ( canTransform() ) {
  	  	  print ("Projecting map using albers equal area projection. This may take some time...")
  	  	  print(Sys.time())
  	 
  	  	  bb<-bbox(tmp.shape)
  	  	  newprojstring <- paste("+proj=aea", sep="") 
  	  	#newprojstring <- paste("+proj=aea ","+lat_1=",bb[1,1]," +lat_2=",bb[1,2]," +lat_0=",mean(bb[1,])," +lon_0=",mean(bb[2,])," +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs", sep="") 
  	  	tmp.shape2<-spTransform(tmp.shape, CRS(newprojstring))
  	  	if (inherits(tmp.shape2,"try-error")) {
  	  		warning("transformation failed")
  	  		tmp.longlat<-TRUE
      		} else {
      			tmp.shape<-tmp.shape2
      		}
  	  }
  } 
 

  if (tmp.longlat) {
  	   warning("Using unprojected lat-lon coordinates. Area measures may be inaccurate. Consider installing (http://cran.r-project.org/web/packages/rgdal/) to have BARD re-project the map or transforming it with a separate program. The Albers Equal Areas (aea) projection with state-plane and state scale data in meter units is recommended.")
  }

  
  tmp.polys <-tmp.shape@polygons

  tmp.df <- tmp.shape@data
  
  
  mapid <- NULL 
  if ("ID" %in% names(tmp.df)) {
    mapid<-"ID"
  } else if ("id" %in% names(tmp.df)) {
    mapid<-"id"
  }


  # Note: poly2nb is slow (even after my patches were integrated) so read from a file if present

    
  tmp.nb <- try(read.gal(gal))
  if (inherits(tmp.nb,"try-error")) {
        print(paste("No gal file found:",gal))
        gal <- NULL
  }
  
  
  # not an else, since top condition may change
  if (is.null(gal)) {
  	  
    estmin <- round((length(tmp.polys)*.002)^2 / 60,2)
    

    print(paste("Since no GAL file supplied, generating gal file, this may take a while  ... (estimated ",estmin," minutes)", sep=" "))
    print(paste("Starting", Sys.time(),sep=" "))
    tmp.nb<-myPoly2nb(tmp.polys)    
    print(paste("Ending", Sys.time(),sep=" "))


    
  }
  
  retval<-list(shape=tmp.shape,polys=tmp.polys,nb=tmp.nb,df=tmp.df)

  if (buildIndex) {
    estmin <- round((length(tmp.polys)*.05) / 60,2)
    print(Sys.time())
    print(paste("Building indices which will save a lot of time in perimeter calculations later ... ( estimated ",estmin," minutes)", sep=" "))
    print(paste("Starting", Sys.time(),sep=" "))

  }
  # generate shape info
  
  tmp.info<-genBlockShapeInfo(retval,buildIndex=buildIndex)
  
  if (buildIndex) {
  	      print(paste("Ending", Sys.time(),sep=" "))
  }
  retval<-c(retval,tmp.info)
  
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
  filen<-sub("\\.shp$","",filen,ignore.case=TRUE)
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

myUnionSpatialPolygons<-local( {
	calledPermit <-FALSE
	function(...) {
		if (!calledPermit) {
			if (exists("gpclibPermit")) { gpclibPermit() }
			calledPermit<-TRUE
		}
		return(unionSpatialPolygons(...))
	}
})


genBlockShapeInfo<-function(basemap, buildIndex=TRUE) {
 if (length(grep("+proj=latlon",basemap$shape@proj4string@projargs,ignore.case=TRUE)>0) || length(grep("+proj=longlat",basemap$shape@proj4string@projargs,ignore.case=TRUE)>0))  { tmp.longlat<-TRUE} else {tmp.longlat<-FALSE}
 
 
   
   tmp.perims<-sapply(1:length(basemap$polys), function(x)genPerim(basemap$shape[x,],longlat=tmp.longlat))
   
   # calculate areas of blocks
   area<-function(poly)sum(unlist(sapply(poly@polygons, 
   function(x)sapply(x@Polygons,function(x)x@area))))
   
   tmp.areas<-sapply(1:length(basemap$polys), function(x)area(basemap$shape[x,]))
   
   tmp.bboxs<-sapply(basemap$polys,function(x)bbox(x))
  tmp.centroids<-sapply(basemap$polys,function(x)x@labpt)
  # HMM -- performance of this is 1000x slower ?
  #tmp.bboxs<-sapply(1:dim(tmp.shape)[1],function(x)bbox(tmp.shape[x,]))
  #tmp.centroids<-sapply(1:dim(tmp.shape)[1],function(x)coordinates(tmp.shape[x,]))
  

  
  if (buildIndex) {
   # calculate overlapping perim between each connected block
     basemap$longlat<-tmp.longlat
     basemap$perims<-tmp.perims
     sharelist<-genPerimIndex(basemap)
  } else {
    sharelist<-NULL
  }
     tmp.timestamp<-Sys.time()

   return(list(perims=tmp.perims,sharedPerims=sharelist, areas=tmp.areas, bboxs=tmp.bboxs, perims=tmp.perims, centroids=tmp.centroids, timestamp=tmp.timestamp, longlat=tmp.longlat))
}


# calculate individual block perims
genPerim<-function(poly,longlat=FALSE) {
	sum(sapply(poly@polygons, 
   function(x)sapply(x@Polygons,function(x)LineLength(x@coords,longlat=longlat))))
}
  
genPerimIndex<-function(basemap) {

   sharelist <- unclass(basemap$nb);   attributes(sharelist)<-NULL
   tmp.longlat<-basemap$longlat
   tmp.perims<-basemap$perims
   
   for ( i in seq(1,length.out=length(basemap$polys)) ) {
   	   tmplist=sharelist[[i]]
   	   for (j in seq(1,length.out=length(tmplist))) {
   	   	 tn <- tmplist[j]
   	   	 if (tn>=i) {
   	   	 	usub <- myUnionSpatialPolygons(basemap$shape[c(i,tn),], c(1,1))
   	   	 	totalperim <- genPerim(usub,longlat=tmp.longlat)
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

importBardShape.polyList <-
function(filen, id="BARDPlanID", gal=paste(filen,".GAL",sep=""), wantplan=FALSE) {
  filen<-sub("\\.shp$","",filen,ignore.case=TRUE)
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
    tmp.nb <- myPoly2nb(tmp.polys)
  }
  
    retval<-list(shape=tmp.shape,polys=tmp.polys,nb=tmp.nb,df=tmp.df)
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
	basemap<-convertBaseMap.polylist(basemap)
	
	
	if(is.null(basemap$longlat)) {
		print("Map built in a previous version, adding indices, please wait...")
		tmpinfo<-genBlockShapeInfo(basemap,buildIndex=TRUE)
		basemap<-c(basemap,tmpinfo)
		class(basemap)<-"bardBasemap"
	}
	
	return(basemap)
}





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


as.data.frame.bardBasemap<-function (x, row.names = NULL, optional = FALSE, ...) {
	as.data.frame(x$shape)
}

dim.bardBasemap<-function(x) {
	dim(x$shape)
}

