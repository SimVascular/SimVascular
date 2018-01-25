/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "MeshSim.h"
#include "svMeshSimAdaptorExports.h" // For exports

/* undocumented? */

#ifdef __cplusplus
extern "C" {
#endif

pPList R_verticesLeft( pRegion region );

#ifdef __cplusplus
}
#endif



SV_EXPORT_MESHSIM_ADAPTOR double Entity_shapeFunction(pEntity entity, pEntity element, int p, int ith,
			      double *L);

/*   This function returns the "body" mode for a tetrahedron.
     i,j,k are the highest monomial orders of r,s,t in the resulting
     polynomial order
*/
SV_EXPORT_MESHSIM_ADAPTOR double Bn(int i, int j, int k, double r, double s, double t);
SV_EXPORT_MESHSIM_ADAPTOR double Bn_2ndChunk(int i, int j, int k, double r, double s, double t);
SV_EXPORT_MESHSIM_ADAPTOR double Bn_3rdChunk(int i, int j, int k, double r, double s, double t);
SV_EXPORT_MESHSIM_ADAPTOR double Bn_4thChunk(int i, int j, int k, double r, double s, double t);

/*  This function mode shape for a simplex edge of order ip */
SV_EXPORT_MESHSIM_ADAPTOR double En(int ip, double r, double s);

/*  This function mode shape for a triangular face. i,j are highest
    order of r,s in the polynomial, maple generated code.
*/
SV_EXPORT_MESHSIM_ADAPTOR double Fn(int i, int j, double r, double s);

SV_EXPORT_MESHSIM_ADAPTOR double V_blendOnEntity(pVertex v, pEntity e, double *L);
SV_EXPORT_MESHSIM_ADAPTOR double V_blendIndexed(int i, double *L);
SV_EXPORT_MESHSIM_ADAPTOR double V_blendIndexedOnEdge(int i, double *L);
SV_EXPORT_MESHSIM_ADAPTOR double E_blendOnFace(pEdge edge, pFace face, double *L);
SV_EXPORT_MESHSIM_ADAPTOR double F_edgeBlendTri(int index[2], double *L);
SV_EXPORT_MESHSIM_ADAPTOR double F_edgeBlendQuad(int *index, double *L);
SV_EXPORT_MESHSIM_ADAPTOR double E_blendOnRegion(pEdge edge, pRegion region, double *L);
SV_EXPORT_MESHSIM_ADAPTOR double R_edgeBlendTet(int index[2], double *L);
SV_EXPORT_MESHSIM_ADAPTOR double F_blendOnRegion(pFace face, pRegion region, double *L);
SV_EXPORT_MESHSIM_ADAPTOR int V_index(pVertex v, pEntity ent, int *index);
SV_EXPORT_MESHSIM_ADAPTOR int E_index(pEdge e, pEntity ent, int *index);
SV_EXPORT_MESHSIM_ADAPTOR int F_index(pFace face, pEntity ent, int *index);
SV_EXPORT_MESHSIM_ADAPTOR double E_modeShape(int p, double *L);
SV_EXPORT_MESHSIM_ADAPTOR double F_modeShapeTri(int p, int i, double *L);
SV_EXPORT_MESHSIM_ADAPTOR double F_modeShapeQuad(int p, int i, double *L);
SV_EXPORT_MESHSIM_ADAPTOR double R_modeShapeTet(int p, int i, double *L);
SV_EXPORT_MESHSIM_ADAPTOR double R_modeShapeHex(int p, int i, double *L);

