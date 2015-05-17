/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University, 
 * RPI, Charles Taylor, Ken Jansen, Nathan Wilson, Ken Wang.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:

 * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer. 
 * Redistributions in binary form must reproduce the above copyright 
 * notice, this list of conditions and the following disclaimer in the 
 * documentation and/or other materials provided with the distribution. 
 * Neither the name of the Stanford University or Rensselaer Polytechnic
 * Institute nor the names of its contributors may be used to endorse or
 * promote products derived from this software without specific prior 
 * written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

#include "cvAdaptHelp.h"

double Entity_shapeFunction(pEntity entity, pEntity element, int p, int ith, 
                            double *L) {
  int entType = EN_type(entity);
  int index[4], elType  = EN_type(element),nedges,nfaces;
  double Lf[4],modeShape,blend ;

  if( elType < entType )
    return 0.0;
  else if( entity == element ) {
    if( entType == Tedge ) {
      return -2.0*L[0]*L[1]*E_modeShape(p,L);
    }
    else if( entType == Tface ) {
      if( F_numEdges(static_cast<pFace>(entity)) == 3 ) 
        return L[0]*L[1]*L[2]*F_modeShapeTri(p,ith,L);
      else
        return F_modeShapeQuad(p,ith,L);
    } else if( entType == Tregion ) {
      if( R_numFaces(static_cast<pRegion>(entity)) == 4 )
        return L[0]*L[1]*L[2]*L[3]*R_modeShapeTet(p,ith,L);
      else if( R_numFaces(static_cast<pRegion>(entity)) == 6 )
        return R_modeShapeHex(p,ith,L);
    }
  }

  /* do based on type of entity */
  if( entType == Tvertex ) /* vertex mode */
    return V_blendOnEntity(static_cast<pVertex>(entity), element, L) ;
  else if ( entType == Tedge ) { /* edge mode */
    /* determine the local index of the edge in this "element" */
    if( !E_index(static_cast<pEdge>(entity),element,index) )
      return 0.0;

    /* get the edge-->element blend */
    if( elType == Tface ) {
      nedges = F_numEdges(static_cast<pFace>(element)) ;
      if( nedges == 3 )
        blend = F_edgeBlendTri(index,L) ;
      else if( nedges == 4 )
        blend = F_edgeBlendQuad(index,L) ;
    } else if ( elType == Tregion ) {
      nfaces = R_numFaces(static_cast<pRegion>(element)) ;
      if( nfaces == 4 )
        blend = R_edgeBlendTet(index,L) ;
    }
    Lf[0] = L[index[0]] ;
    Lf[1] = L[index[1]] ;
    /* get the shape function */
    return blend*E_modeShape(p,Lf) ;
  } else if ( entType == Tface ) {
    /* only works for triangular faces of a tet. right now */
    if( F_numEdges(static_cast<pFace>(entity)) != 3  || R_numFaces(static_cast<pRegion>(element)) != 4 ) 
      return 0.0;  

    /* determine the local indices of vertices of the face in this "element" */
    if( !F_index(static_cast<pFace>(entity),element,index) )
      return 0.0;

    Lf[0] = L[index[0]];
    Lf[1] = L[index[1]];
    Lf[2] = L[index[2]];
    
    /* get the mode shape of the face */
    modeShape = Lf[0]*Lf[1]*Lf[2]*F_modeShapeTri(p,ith,Lf);
    
    return modeShape ;
  } else
    return 0.0;
}


/* determine the local vertex index of vertices defining the given
   mesh entity within the mesh database. */

int V_index(pVertex v, pEntity ent, int *index) {
  int retval = 1;
  void *temp ;
  int nverts,type = EN_type(ent);
  pVertex vert;
  pPList verts;

  if( type == Tvertex )
    retval = 0;
  else if( type == Tedge ) {
    if( E_vertex(static_cast<pEdge>(ent),0) == v )
      index[0] = 0;
    else if( E_vertex(static_cast<pEdge>(ent),1) == v )
      index[0] = 1;
    else
      retval = 0;
  } else {
    if( type == Tface )
      verts = F_vertices(static_cast<pFace>(ent),1);
    else
      verts = R_vertices(static_cast<pRegion>(ent),1);
      //verts = R_verticesLeft(static_cast<pRegion>(ent));

    nverts = PList_size(verts);
    *index = retval = 0 ;
    temp = 0;
    while((vert=(pVertex)PList_next(verts,&temp))) {
      if( vert == v ) {
        retval = 1;
        break;
      }
      else
        (*index)++;
    }
    PList_delete(verts);
  }
  return retval;
}

int E_index(pEdge e, pEntity ent, int *index) {
  pVertex ev,vert;
  pPList verts ;
  void *temp;
  int i,count,retval=1,nverts,type;
  static pEntity lastElement,lastEntity;
  static int lastIndex[2],lastReturn;

  /* if this is the same element entity-element pair dont do the 
     search again 
  */
  if( lastEntity == e && lastElement == ent ) {
    index[0] = lastIndex[0];
    index[1] = lastIndex[1];
    return lastReturn;
  }

  type = EN_type(ent);

  if( type < Tface ) 
    retval = 0;
  else {
    if(type == Tface)
      verts = F_vertices(static_cast<pFace>(ent),1);
    else
      verts = R_vertices(static_cast<pRegion>(ent),1);
    //verts = R_verticesLeft(static_cast<pRegion>(ent));
    nverts = PList_size(verts);
    for(i=0; i<2; i++) {
      ev = E_vertex(e,i);
      index[i] = retval = 0;
      temp = 0;
      while((vert=static_cast<pVertex>(PList_next(verts,&temp)))) {
        if( vert == ev) {
          retval = 1;
          break;
	} else
          index[i]++;
      }
      if(!retval)
        break ;
    } 
    PList_delete(verts);
  }
  lastElement = ent;
  lastEntity  = e;
  lastIndex[0] = index[0];
  lastIndex[1] = index[1];
  lastReturn  = retval;
  
  return retval;
}

int F_index(pFace face, pEntity ent, int *index) {
  int type,nverts,count,fcount,retval=1 ;
  pPList fverts,verts;
  pVertex v,fv;
  void *temp,*ftemp;
  static pEntity lastElement,lastEntity;
  static int lastIndex[3],lastReturn;

  /* if this is the same element entity-element pair dont do the 
     search again 
  */
  if( lastEntity == face && lastElement == ent ) {
    index[0] = lastIndex[0];
    index[1] = lastIndex[1];
    index[2] = lastIndex[2];
    return lastReturn;
  }

  type = EN_type(ent);
  if( type == Tregion ) {
    fverts = F_vertices(face,1);
    verts = R_vertices(static_cast<pRegion>(ent),1);
    //verts = R_verticesLeft(static_cast<pRegion>(ent));
    nverts = PList_size(verts);
    fcount =0;
    ftemp=0;
    while((fv=(pVertex)PList_next(fverts,&ftemp))) {
      temp = 0;
      count =0;
      while((v=(pVertex)PList_next(verts,&temp))) {
        if( v == fv )
          break;
        else
          count++;
      }
      if( count < nverts )
        index[fcount++] = count;
      else {
        retval = 0;
        break;
      }
    }
    PList_delete(fverts);
    PList_delete(verts);
  } else
    retval = 0;

  lastElement = ent;
  lastEntity  = face;
  lastIndex[0] = index[0];
  lastIndex[1] = index[1];
  lastIndex[2] = index[2];
  lastReturn  = retval;

  return retval ;
}

/*
  THis function evaluate the blend funtion for a given mesh entity over
  another mesh entity
*/

double V_blendOnEntity(pVertex v, pEntity e, double *L) {
  /* blend a vertex mode on a mesh edge */
  int index ;

  if( V_index(v,e,&index) ) {
    if( EN_type(e) == Tedge )
      return V_blendIndexedOnEdge(index,L);
    else
      return V_blendIndexed(index,L);
  }
  else
    return 0.0;
}

double V_blendIndexed(int i, double *L) {
  return L[i] ;
}

double V_blendIndexedOnEdge(int i, double *L) {
  if( i == 0 )
    return 0.5*(1.0-L[0]);
  else
    return 0.5*(1.0+L[0]);
}

double E_blendOnFace(pEdge edge, pFace face, double *L) {
  /* blend an edge mode on a tri. face */
  int index[2],nedges = F_numEdges(face);

  if(!E_index(edge, face, index) )
    return 0.0;

  /* find out the local edge index in the face */
  if( nedges == 3 ){ 
    return F_edgeBlendTri(index, L);
  } else if( nedges == 4 ){
    return F_edgeBlendQuad(index, L);
  } else
    return 0.0;
}

double F_edgeBlendTri(int index[2], double *L) {
  return -2.0*L[index[0]]*L[index[1]];
}

double F_edgeBlendQuad(int *index, double *L) {
  return 0.0;
}

double E_blendOnRegion(pEdge edge, pRegion region, double *L) {
  /* blend a mesh edge mode on a tetra. region */
  int index[2],nfaces= R_numFaces(region);
  
  /* figure our which local edge we are dealing with */
  if(!E_index(edge, region, index) )
    return 0.0;
 if( nfaces == 4 ) 
    return R_edgeBlendTet(index, L);   
 else
  return 0.0;
}

double R_edgeBlendTet(int index[2], double *L) {
  return -2.0*L[index[0]]*L[index[1]];
}

double F_blendOnRegion(pFace face, pRegion region, double *L) {
  int index[4] ;

  /* blend a face mode on a tet. region */
  int nfaces = R_numFaces(region);

  if(!F_index(face, region, index)) 
   return 0.0;

  if( nfaces == 4 ) {
    return L[index[0]]*L[index[1]]*L[index[2]] ;
  } else
    return 0.0;
}
