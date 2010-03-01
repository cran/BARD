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

