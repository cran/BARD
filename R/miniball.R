#########################################################################
#
# miniball.R
#
# Computes the minimum volume ball enclosig a set of points
#
#    Copyright (C) 2004  Micah Altman
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##########################################################################

# Note: this version of miniball C++ library requires a fixed number of 
# dimensions. New versions will be more flexible

MINIBALLMAXDIM<-2

##########################################################################
#
#  miniball
#
#  An R wrapper for the minball c++ library
#
#  See the .Rd files for documentation
##########################################################################

"miniball" <-
function(points,pivot=TRUE,distances=FALSE) {

	if (! is.matrix(points)) {
		warning("points must be matrix");
		return(NULL);
	}
	if (dim(points)[2] > MINIBALLMAXDIM ) {
		warning("too many dimensions");
		return(NULL);
	}
	points=na.omit(points)
	
	
	r = .C("R_miniball", NAOK=FALSE, 
		PACKAGE="BARD", 
		support=points,
		r=as.integer(dim(points)[2]),
		n=as.integer(dim(points)[1]),
		accuracy=double(1),
		center=double(dim(points)[2]),
		slack=double(1),
		sqradius=double(1),
		pivot=as.integer(pivot),
		numSupportPoints=integer(1)
	   )
  #  TRICKY -- We are overwriting the first N Rpoints as the return value, so we
  #   truncate
   r[[1]]<-r[[1]][1:r$numSupportPoints, ] 
  if (distances) {
	r$distances <- rowSums(t(t(points) - r$center)^2)
  }
	class(r)="ball";	
	return(r);
}
