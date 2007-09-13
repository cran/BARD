/*
 * R_miniball.C
 * 
 * R wrapper for miniball 
 * 
 * Available from www.r-project.org and
 * www.hmdc.harvard.edu/numerical_issues/
 * 
 *    Copyright (C) 2004  Micah Altman
 *
 *    This program is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */


#include "miniball_config.h"
#include "miniball.h"
#include <R.h>

#define MAXD 2
// extern "C" so that R can see the functions...
extern "C"  {



void R_miniball(double Rpoints[], int *Rd, int *Rn, 
	 double *Raccuracy, double Rcenter[], double *Rslack, double *Rsqradius ,
	int *pivot, int *Rsupport	
) {

	Miniball<MAXD>     mb;
	Miniball<MAXD>::Point p;
	for (int i=0; i<*Rn; ++i) {
     for (int j=0; j<*Rd; ++j) {
        p[j] = Rpoints[i+(*Rn)*j];
		}
    for(int j=*Rd; j<MAXD; j++) {
		   p[j]=0;
    }
               mb.check_in(p);
        }

	 // Find the ball
	 mb.build(*pivot);

   // return values
   Miniball<MAXD>::Point p2;
   p2= mb.center();
	 for (int i=0; i<*Rd; i++) {
		  Rcenter[i]=p2[i];
	 }
	 *Rsqradius=	mb.squared_radius();
	 mbdouble tslack;
   *Raccuracy = (double)  mb.accuracy (tslack); // changes slack, tricky
  	*Rslack= (double) tslack;
     //  number of support points
    // -------------------------------
    *Rsupport= mb.nr_support_points() ;
   
    //  support points
    // ---------------------
  
    //  TRICKY -- We are overwriting the first N Rpoints as the return value!
    Miniball<MAXD>::Cit it;
    int i=0;
    for (it=mb.support_points_begin(); it!=mb.support_points_end(); ++it) {
      i++;
      for (int j=0; j<*Rd; ++j) {
        Rpoints[i+(*Rn)*j]=(*it)[j];
		  }
     }
	
 	
  }
  


} // extern "C"
   

   


