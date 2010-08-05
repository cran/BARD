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

# Portions modified from the spdep library, Copyright 2001-6 by Roger S. Bivand.  



myPoly2nb.internal <- function(pl, row.names=NULL, snap=sqrt(.Machine$double.eps),
queen=TRUE) {
	if (!inherits(pl, "polylist")) {
		if (extends(class(pl), "SpatialPolygons"))
		pl <- maptools:::.SpP2polylist(pl)
		else stop("Not a polygon list")
	}
	if (inherits(pl, "multiparts")) stop("Convert to newer polylist format")
	n <- length(pl)
	if (n < 1) stop("non-positive number of entities")
	if (is.null(row.names)) regid <- attr(pl, "region.id")
	else regid <- NULL
	if (is.null(regid)) {
		if(is.null(row.names)) regid <- as.character(1:n)
		else {
			if(length(row.names) != n)
			stop("row.names wrong length")
			else if (length(unique(row.names)) != length(row.names))
			stop("non-unique row.names given")
			else regid <- row.names
		}
	}
	
	genBBIndex<-function(pl,snap=sqrt(.Machine$double.eps)) { 
		poly2bbs <- function(pl) {
			n <- length(pl)
			if (n < 1)
            stop("non-positive number of entities")
			res <- matrix(0, nrow = n, ncol = 4)
			for (i in 1:n) res[i, ] <- attr(pl[[i]], "bbox")
			res
		}
		bb<-poly2bbs(pl)
		if (storage.mode(bb) != "double")
        storage.mode(bb) <- "double"
		dsnap <- as.double(snap)
		bb[, 1] <- bb[, 1] - dsnap
		bb[, 2] <- bb[, 2] - dsnap
		bb[, 3] <- bb[, 3] + dsnap
		bb[, 4] <- bb[, 4] + dsnap
		
		bxv <- as.vector(bb[,c(1,3)])
		byv <- as.vector(bb[,c(2,4)])
		obxv <- order(bxv)
		rbxv <- c(1:(length(pl)*2))[obxv]
		mbxv <- match(1:(length(pl)*2),obxv)
		obyv <- order(byv)
		rbyv <- c(1:(length(pl)*2))[obyv]
		mbyv <- match(1:(length(pl)*2),obyv)
		
		return(list(bb=bb,bxv=bxv,byv=byv,obxv=obxv,obyv=obyv,mbxv=mbxv,mbyv=mbyv,rbyv=rbyv,rbxv=rbxv))
	}
	BBindex<-genBBIndex(pl)
	bb<-BBindex$bb
	
	dsnap <- as.double(snap)
	nrs <- integer(n)
	for (i in 1:n) {
		pl[[i]] <- na.omit(pl[[i]][-1,])
		nrs[i] <- as.integer(nrow(pl[[i]]))
		pl[[i]] <- as.double(pl[[i]])
	}
	
	qintersect<-function(x,y) {
# streamlined intersect function for unique vectors
		y[match(x, y, 0L)]
	}
	findInBox<-function(i,sp,bigger=TRUE) {
		n<-dim(sp$bb)[1]
		
# use index structure to identify which other BB's fall in i's BB
# by getting id's of polygons with BBmin_j < BBmax_i, BBmax_j > BBmin_i for x and y 
# then taking the intersection of these four lists of id's
		
		tmp<-vector(mode="list", length=4)
# ! i1 > j3 --> i1 <= j3
		tmp[[1]] <- sp$rbxv[sp$mbxv[i]:(n*2)]
		tmp[[1]]<- tmp[[1]][which(tmp[[1]]>n)] - n
# ! i2 > j4 --> i2 <= bj4
		tmp[[2]] <- sp$rbyv[sp$mbyv[i]:(n*2)]
		tmp[[2]]<- tmp[[2]][which(tmp[[2]]>n)] - n
# ! i3 < j1 -> i3 >= j1
		tmp[[3]] <- sp$rbxv[1:sp$mbxv[i+n]]
		tmp[[3]] <- tmp[[3]][which(tmp[[3]]<=n)]
# ! i4 < j2 -> i4 >= j2
		tmp[[4]] <- sp$rbyv[1:sp$mbyv[i+n]]
		tmp[[4]]<- tmp[[4]][which(tmp[[4]]<=n)]
		
# for performance, order the comparison of the lists
		
		lentmp <- order(sapply(tmp,length))
		
# use qintersect, since these are already vectors and unique 
		result <- qintersect(tmp[[lentmp[2]]],tmp[[lentmp[1]]])
		result <- qintersect(tmp[[lentmp[3]]],result)
		result <- qintersect(tmp[[lentmp[4]]],result)
		
		if (bigger) {
			result<-result[which(result>i)]
		}
		return(sort(result))
	}
	
	
	polypoly2 <- function(poly1, nrs1, poly2, nrs2, snap) {
		if (any(nrs1 == 0 || nrs2 == 0)) return(as.integer(0))
		res <- .Call("BARDpolypoly", poly1, nrs1, poly2, 
					 nrs2, snap, PACKAGE="BARD")
		res
	}
	
	ans <- vector(mode="list", length=n)
	for (i in 1:n) ans[[i]] <- integer(0)
	criterion <- ifelse(queen, 0, 1)
	for (i in 1:(n-1)) {
#for (j in (i+1):n) {
		for (j in findInBox(i,BBindex)) {
			jhit <- .Call("BARDoverlap", bb[i,], 
						  bb[j,], PACKAGE="BARD")
			if (jhit > 0) {
			    khit <- 0
			    khit <- polypoly2(pl[[i]], nrs[i], pl[[j]], 
								  nrs[j],dsnap)
				
			    if (khit > criterion) {
					ans[[i]] <- c(ans[[i]], j)
					ans[[j]] <- c(ans[[j]], i)
			    }
			}
		}
	}
	for (i in 1:n) ans[[i]] <- sort(ans[[i]])
	class(ans) <- "nb"
	attr(ans, "region.id") <- regid
	attr(ans, "call") <- match.call()
	if (queen) attr(ans, "type") <- "queen"
	else attr(ans, "type") <- "rook"
	ans <- sym.attr.nb(ans)
	ans
}







#myPoly2nb <- spdep::poly2nb 
myPoly2nb <- myPoly2nb.internal
