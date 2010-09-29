##############################################################################
#
# Interactive Plan editing and manipulation
#
# This uses imap() (part of iplots) and tk widgets to do interactive selection
# of plans
#
#
# Very cool, but lots of caveats:
#     - iplots is somewhat unstable. Quitting from the menu does bad things
#     - avoid the iplot event loop, if users close the map window withough
#       explicitly breaking R hangds
#     - sometimes iplot() crashes R
#     - performance on large maps will not be happy
#     - interface for selecting/assigning/evaluating could be better integrated
#        (currently, no way to allow pop up menu to assign district, position
#         where windows initially appear, etc)
#        
#
#############################################################################


#################################
#
# editPlanInteractive
#
# This is the workhorse function for interactive editing
# After every change, a reporting function is run with the current plan
#
# Arguments
#
#   - plan - the plan to be edited
#   - reportFUN - a function to be run to generate reports on the plan
#   - ... - args to pass to reportFUN
#
# Returns
#    - returns new plan
#
# SEE R HELP FILE  FOR FULL DETAILS
#
##################################

 editPlanInteractive<-function(plan,reportFUN=NULL,...){
  basemap<-basem(plan)
  ndists<-attr(plan,"ndists")
  #interactive setup

  icheck<-checkIplots()
  if((icheck!=TRUE)) {
    warning(icheck)
    return(plan)
  }

  ### Workaround for codetools warnings in R CMD check
  imap<-try(imap,silent=TRUE)
  if (inherits(imap,"try-error")){
	imap<-function(...){}
        iset.col<-iplot.off<-iplot.opt<-iset.selectNone<-iset.selected<-iset.col<-imap 
  }
  ### end workaround

  if(!mrequire("tcltk", quietly = TRUE, warn.conflicts=FALSE)) {
    warning("This requires the tcltk package. Please install it.")
    return(plan)
  } else {
    if (!exists("tk_select.list")) {
	# we never get here...
	# avoid complaints from code checker
	tk_select.list<-function(...){}
    }
  } 
  if (!interactive()) {
    warning("Can't edit plans interactively, if not in an interactive session")
    return(plan)
  }
    
  pmap<-Polygons2map(basePolys(basemap))
  choice<-0
  im<-imap(pmap)
  on.exit(iplot.off(im))
  iplot.opt(col=plan+2)
  iset.selectNone()
 
  
  # selection loop
  while(choice!="DONE") {
    choice<-tk_select.list(c(as.character(1:ndists),"DONE"),
        title="Assign to district")

    if (is.na(as.numeric(choice))) {
        choice<-"DONE"
    } else {
 
    	selected<-unique(as.numeric(pmap$names[iset.selected()]))
    	plan[selected]<-as.numeric(choice)

        
        iset.selectNone()
        iset.col(plan+2)
        if (!is.null(reportFUN)) {
          reportFUN(plan,...)
          flush.console()
        }        
    }
  }
  return(plan)
}

#################################
#
# createPlanInteractive
#
# This creates a plan interactively
# Essentially a wrapper around editPlanInteractive
#
# Arguments
#
#   - basemap- basemap
#   - ndists- number of dists
#   - other args as per editPlanInteractive
#
# Returns
#    - returns new plan
#
# SEE R HELP FILE  FOR FULL DETAILS
#
##################################

createPlanInteractive<-function(basemap,ndists,reportFUN=NULL,...) {
  # plan setup
  nblocks<-dim(basemap)[1]
  plan<-integer(nblocks)
  plan[which(plan==0)]<-1
  attr(plan,"ndists")<-ndists
  basem(plan)<-basemap
  class(plan)<-"bardPlan"
  retval<-editPlanInteractive(plan,reportFUN,...)
}

#################################
#
# polys2map
#
# Creates a "map" object from a polylist
#
# Note: This is a "map" object in the form expected by the 
#   	"maps" package -- i.e. splus 3 style. It is not a
#	"Map" as per "maptools" or per ade pacakges.
#
# Arguments
#
#   -  polys - polylist object
#
# Returns
#    - returns map object
#
# SEE R HELP FILE  FOR FULL DETAILS
#
##################################


polys2map<-function(polys){
  mapx<-c(sapply(polys,function(x)c(x[,1],Inf)),recursive=TRUE)
  mapy<-c(sapply(polys,function(x)c(x[,2],Inf)),recursive=TRUE)
  missx<-which(is.na(mapx))
  missy<-which(is.na(mapy))
  missxy<-unique(c(missx,missy))
  if (length(missxy)>0) {
  	  mapx<-mapx[-missxy]
  	  mapy<-mapy[-missxy]
  }
  is.na(mapx[which(is.infinite(mapx))])<-TRUE
  is.na(mapy[which(is.infinite(mapy))])<-TRUE
  retval<-list()
  retval$x<-mapx
  retval$y<-mapy
  retval$range<-c(min(mapx,na.rm=T),max(mapx,na.rm=T),min(mapy,na.rm=T),max(mapy,na.rm=T))
  mapnames<-sapply(polys,function(x)attr(x,"shpID"))
  tmpw<-which(is.na(mapnames))
  mapnames[tmpw]<-as.character(tmpw)
  retval$names<-mapnames
  class(retval)="map"
  return(retval)
}

# for newer list of Polygons S4 class

Polygons2map<-function(polys){
  Polygon2shape<-function(p,d) {
  	retval<-NULL
  	if (!p@hole) {
  		retval<-p@coords[,d]
  		} else {
  		
  		#retval<-rev(p@coords[,d])
  	}
  	#if (!p@hole) {
  	#	retval<-c(retval,Inf)
  	#}
  	
  	return(retval)
  }
  
  mapx<-unlist(sapply(polys,function(x)c(sapply(x@Polygons,function(x)Polygon2shape(x,1)),Inf)))
  mapy<-unlist(sapply(polys,function(x)c(sapply(x@Polygons,function(x)Polygon2shape(x,2)),Inf)))
  missx<-which(is.na(mapx))
  missy<-which(is.na(mapy))
  missxy<-unique(c(missx,missy))
  if (length(missxy)>0) {
  	  mapx<-mapx[-missxy]
  	  mapy<-mapy[-missxy]
  }
  is.na(mapx[which(is.infinite(mapx))])<-TRUE
  is.na(mapy[which(is.infinite(mapy))])<-TRUE
  retval<-list()
  retval$x<-mapx
  retval$y<-mapy
  retval$range<-c(min(mapx,na.rm=T),max(mapx,na.rm=T),min(mapy,na.rm=T),max(mapy,na.rm=T))
  mapnames<-unlist(sapply(1:length(polys),function(x)replicate(length(polys[[x]]@Polygons),x)))
  mapnames<-as.character(mapnames)
  retval$names<-mapnames
  class(retval)="map"
  return(retval)
}



####
#
#	Check iplots
#
#	Iplots won't run on MacOS outside of JGR, this requires some
#	checking...
#
####

checkIplots<-function() {
   # special OSX
   if (!mrequire("rJava", quietly = TRUE, warn.conflicts=FALSE)) {
  	return("rJava must be installed")
   }
   if (length(grep("^darwin",R.version$os))) {
   	success<-TRUE
	try ( silent = TRUE, {
		.jinit()
		 if (!any(.jcall("java/lang/System","S","getProperty","main.class")=="org.rosuda.JGR.JGR")) {
			
		 	success<-("Iplots on MacOS must be run under JGR. Please install and launch JGR and try again.")
		}

	})
	if (!is.logical(success)) return(success)
   }

  # normal ...
  if(!mrequire("iplots", quietly = TRUE, warn.conflicts=FALSE)) {
	return("iplots is not installed")
  }
  return(TRUE)
}
