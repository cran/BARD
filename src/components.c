/* Copyright 2001 by Nicholas Lewin-Koh. */
/*  Modified 2007 by Micah Altman to handle an inclusion list,
    this is MUCH faster than using subset.nb and  n.comp.nb in combination */

#include<R.h>
#include<Rinternals.h>

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
