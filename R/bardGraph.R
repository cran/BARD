##############################################################################
#
# bard mathematical graph functions
#
# 
# This module contains methods for dealing with graphs and adjacency lists
#
#############################################################################


##############################################################################
#
# nb2graph
#
# This converts a nb into a graph object, used in the "graph" and "RGBL"
# libraries. This allows for the use of the high performance graph
# algorithms in these libraries
#
# Arguments:
#     nb - neighborhood list from nbdep
#
# Returns
#     graphNEL representation (if graph module available), matrix otherwise
#
# WARNING: This uses a matrix as an intermediate step, and can require a lot
#          of memory for the transformation
#
# SEE R HELP FILE  FOR FULL DETAILS
#
##############################################################################

"nb2graph" <-
function(nb) {
  tmp.mat <- nb2mat(nb,style="B",zero.policy=TRUE)
  rownames(tmp.mat)<-colnames(tmp.mat)<-attr(nb,"region.id")
  if(!mrequire("graph",quietly=TRUE)) {
    warning("Graph module not available, returning ordinary matrix")
    return(tmp.mat)
  }
  return(as(tmp.mat,"graphNEL"))
}

##############################################################################
#
# neighbors
#
# A generic method, returns the neighboring elements to node i in a graph
#
# Arguments:
#     i - node identifiers
#
# Returns
#     vector of nodes that are adjacent to the nodes given
#
# SEE R HELP FILE  FOR FULL DETAILS
#
##############################################################################

"neighbors" <-
function(nb,i,...) {
  UseMethod("neighbors")
}

"neighbors.nb" <-
function(nb,i,...) {
  sort(unique(unlist(nb[i])))
}

##############################################################################
#
# n.comp.include
#
# Returns number of unconnected component in a neighborhood list,
# including only particular elements.
#
# This is similar to n.comp.nb(subset(nb,include)), but 20x faster
#
# Arguments:
#     nb.obj - neighborhood list
#     include - logical vector of length(nb.obj), or list of id's to include
#
# Returns
#   list of
#     nc -  number of connected components,
#     comp.id - vector of indices of disjoint connected subgraphs , 0 indicates
#       id was excluded
#
# SEE R HELP FILE  FOR FULL DETAILS
#
##############################################################################

n.comp.include<-function (nb,include) 
{
nb.obj<-nb
if (!inherits(nb.obj, "nb")) 
stop("not a neighbours list")
if (length(include)!=length(nb.obj) && !is.logical(include) ) {
tmpinclude<-logical(length(nb.obj))
tmpinclude[intersect(1:length(nb.obj),include)]<-TRUE
} else {
tmpinclude<-include
}
nb.obj <- make.sym.nb(nb.obj)
comp <- rep(0, length(nb.obj))
comp <- .Call("g_components_d", nb.obj, as.integer(comp), as.integer(tmpinclude),PACKAGE = "BARD")

retval <- list(nc = length(setdiff(unique(comp),0)), comp.id = comp)
return(retval)
}

###
### Optimized poly2nb routine, uses a basic spatial index instead of comparing all blocks
###

#myPoly2nb <- spdep::poly2nb 
rgeostatus<-local ({
	rgeoschecked<-NULL
	calledPermit <-FALSE
	
	function (set=-1) {
		if (set!=-1) {
			rgeoschecked<<-set
		} 
			
		if (is.null(rgeoschecked)) {
			  ow<-options(warn=-1)
			  #geostatus<-maptools:::rgeosStatus()
			  rgeoschecked <<- mrequire("rgeos", quietly = TRUE,warn.conflicts = FALSE)
			  # Don't do this yet...maptools still stubbed out 
			  #try(assign("rgeos", rgeoschecked, envir=maptools:::.MAPTOOLS_CACHE),silent=TRUE)
			  options(ow)
		}
		
		if(!rgeoschecked) {
			if (!calledPermit) {
				if (exists("gpclibPermit")) { gpclibPermit() }
				calledPermit<<-TRUE
			}
		}
		
		return(rgeoschecked)
	}
})

myPoly2nb<-function (pl, row.names = NULL, snap=sqrt(.Machine$double.eps),
	 queen=TRUE, useC=TRUE, foundInBox=NULL, repair=TRUE) {
	
        ow<-options(warn=-1)
	options(ow)
	#if (is.null(foundInBox) && rgeostatus()) {
	#	foundInBox <- poly_findInBoxGEOS(pl)
	#}
	res<-poly2nb(pl=pl,row.names=row.names,snap=snap,queen=queen,useC=useC,foundInBox=foundInBox)
	zw<-zeroWeights.nb(res)
	if (length(zw)>0) {
		res2<-res
		warning(paste("Discontinuities detected, linking the following units to the block with closest centroid: ",paste(zw,collapse="") ))
		centroids<-sapply(pl@polygons,function(x)x@labpt)
		for (i in zw) {
			tmpd<-sapply(1:length(res),function(x)pdist(centroids[,i],centroids[,x]))
			tmpd[i]<-Inf
			res[i]<-which.min(tmpd)	
		}
		
	}
	return (res)
}


zeroWeights.nb<-function(nb) {
	which(sapply(nb,function(x)((length(x)==1)&&(x==0))))
}

pdist<-function(x,y) { (x[1]-y[1])^2+ (x[2]-y[2])^2}


