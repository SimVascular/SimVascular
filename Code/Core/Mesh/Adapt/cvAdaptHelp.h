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

#include "MeshSim.h"

/* undocumented? */

#ifdef __cplusplus
extern "C" {
#endif

pPList R_verticesLeft( pRegion region );

#ifdef __cplusplus
}
#endif



  double Entity_shapeFunction(pEntity entity, pEntity element, int p, int ith, 
			      double *L); 

/*   This function returns the "body" mode for a tetrahedron.
     i,j,k are the highest monomial orders of r,s,t in the resulting
     polynomial order
*/
  double Bn(int i, int j, int k, double r, double s, double t);
  double Bn_2ndChunk(int i, int j, int k, double r, double s, double t);
  double Bn_3rdChunk(int i, int j, int k, double r, double s, double t);
  double Bn_4thChunk(int i, int j, int k, double r, double s, double t);

/*  This function mode shape for a simplex edge of order ip */
  double En(int ip, double r, double s);

/*  This function mode shape for a triangular face. i,j are highest
    order of r,s in the polynomial, maple generated code.
*/
  double Fn(int i, int j, double r, double s);

  double V_blendOnEntity(pVertex v, pEntity e, double *L);
  double V_blendIndexed(int i, double *L);
  double V_blendIndexedOnEdge(int i, double *L);
  double E_blendOnFace(pEdge edge, pFace face, double *L); 
  double F_edgeBlendTri(int index[2], double *L);
  double F_edgeBlendQuad(int *index, double *L); 
  double E_blendOnRegion(pEdge edge, pRegion region, double *L);
  double R_edgeBlendTet(int index[2], double *L);
  double F_blendOnRegion(pFace face, pRegion region, double *L);
  int V_index(pVertex v, pEntity ent, int *index);
  int E_index(pEdge e, pEntity ent, int *index);
  int F_index(pFace face, pEntity ent, int *index);
  double E_modeShape(int p, double *L);
  double F_modeShapeTri(int p, int i, double *L);
  double F_modeShapeQuad(int p, int i, double *L);
  double R_modeShapeTet(int p, int i, double *L);
  double R_modeShapeHex(int p, int i, double *L);

