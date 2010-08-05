/* Copyright 2001 by Nicholas Lewin-Koh. */
/*  Modified 2007 by Micah Altman to handle an inclusion list,
    this is MUCH faster than using subset.nb and  n.comp.nb in combination */

#include<R.h>
#include<Rinternals.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>
#define ROFFSET 1


#define BLACK 1
#define WHITE 0

void dfs(SEXP nblst, SEXP cmpnm, SEXP visited, int curcmp, int nodeid){
  int n,i;

  INTEGER(cmpnm)[nodeid]=curcmp;
  INTEGER(visited)[nodeid]=BLACK;
  n=length(VECTOR_ELT(nblst,nodeid));

  for(i=0;i<n;i++){
    if(INTEGER(visited)[(INTEGER(VECTOR_ELT(nblst,nodeid))[i]-1)]==WHITE){ 
      dfs(nblst, cmpnm, visited, curcmp,
	  INTEGER(VECTOR_ELT(nblst,nodeid))[i]-1 );
    }
  }
}


SEXP g_components_d(SEXP nblst, SEXP cmpnm, SEXP include){
  int i, curcmp=1, nvert;
  SEXP visited;
  
  nvert=length(nblst);
  PROTECT(visited=allocVector(INTSXP,nvert));
  
  for(i=0; i < nvert; i++){
    if(INTEGER(include)[i]==0) {
       INTEGER(visited)[i]=BLACK;
    } else {
      INTEGER(visited)[i]=WHITE;
    }
  }

  for(i=0; i < nvert; i++){
    if(INTEGER(visited)[i]==WHITE){ 
      INTEGER(visited)[i]=BLACK;
      if(INTEGER(VECTOR_ELT(nblst,i))[0]==0){
	INTEGER(cmpnm)[i]=curcmp;
	curcmp++;
      }
      else{
	dfs(nblst, cmpnm, visited, curcmp, i);
	curcmp++;
      }
    }    
  }
  UNPROTECT(1);
  return(cmpnm);
}

/* Optimized routines for internal poly2nb */
/* Portions modified from the spdep library, Copyright 2004 by Roger S. Bivand.  */


SEXP BARDpolypoly(SEXP p1, SEXP n01, SEXP p2, SEXP n02, SEXP snap);
SEXP BARDoverlap(SEXP bbbi, SEXP bbbj);

SEXP BARDpolypoly(SEXP p1, SEXP n01, SEXP p2, SEXP n02, SEXP snap)
{
	int n1=INTEGER_POINTER(n01)[0], n2=INTEGER_POINTER(n02)[0], pc=0;
	int i, j, k=0;
	double sn=NUMERIC_POINTER(snap)[0], dist;
	double x1, x2, y1, y2, xd,yd, sn2 =sn*sn;
	
	SEXP ans;
	PROTECT(ans = NEW_INTEGER(1)); pc++;
	
	
	for (i=0; (i < n1) && (k < 2); i++) {
		x1 = NUMERIC_POINTER(p1)[i];
		y1 = NUMERIC_POINTER(p1)[n1 + i];
		for (j=0; (j < n2) && (k < 2); j++) {
			x2 = NUMERIC_POINTER(p2)[j];
			y2 = NUMERIC_POINTER(p2)[n2 + j];
			xd = x1-x2;
			if (fabs(xd)>sn) { continue; }
			yd = y1-y2;
			if (fabs(yd)>sn) { continue; }
			dist = xd*xd + yd*yd;
			if (dist < sn2) k++;
		}
	} 
	
	INTEGER_POINTER(ans)[0] = k;
	
	UNPROTECT(pc); /* ans */
	return(ans);
}

SEXP BARDoverlap(SEXP bbbi, SEXP bbbj) {
	
	int pc=0,overlap=1;
	double bbi[4], bbj[4];
	SEXP ans;
	
	PROTECT(ans = NEW_INTEGER(1)); pc++;
	bbi[0] = NUMERIC_POINTER(bbbi)[0];
	bbi[1] = NUMERIC_POINTER(bbbi)[1];
	bbi[2] = NUMERIC_POINTER(bbbi)[2];
	bbi[3] = NUMERIC_POINTER(bbbi)[3];
	bbj[0] = NUMERIC_POINTER(bbbj)[0];
	bbj[1] = NUMERIC_POINTER(bbbj)[1];
	bbj[2] = NUMERIC_POINTER(bbbj)[2];
	bbj[3] = NUMERIC_POINTER(bbbj)[3];
	
	if ((bbi[0]>bbj[2]) | (bbi[1]>bbj[3]) | 
		(bbi[2]<bbj[0]) | (bbi[3]<bbj[1]) ) {
		overlap=0;
	}
	
	INTEGER_POINTER(ans)[0] = overlap;		
	UNPROTECT(pc); /* ans */
	return(ans);
}


void BARDfindInBox (int rbxv[], int rbyv[], int mxbv[], int mybv[],
					int selected[], int *len ) {
	
	int i;
	/*tmp[[1]] <- sp$rbxv[sp$mbxv[i]:(n * 2)]
	tmp[[1]] <- tmp[[1]][which(tmp[[1]] > n)] - n
	tmp[[2]] <- sp$rbyv[sp$mbyv[i]:(n * 2)]
	tmp[[2]] <- tmp[[2]][which(tmp[[2]] > n)] - n
	tmp[[3]] <- sp$rbxv[1:sp$mbxv[i + n]]
	tmp[[3]] <- tmp[[3]][which(tmp[[3]] <= n)]
	tmp[[4]] <- sp$rbyv[1:sp$mbyv[i + n]]
	tmp[[4]] <- tmp[[4]][which(tmp[[4]] <= n)]*/
	
	for (i = 0; i < *len; i++) {
		
	}
	
}

