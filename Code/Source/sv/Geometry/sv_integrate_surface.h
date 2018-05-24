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

#ifndef _CVINTEGRATESURFACE_H
#define _CVINTEGRATESURFACE_H

#include "SimVascular.h"
#include "svGeometryExports.h" // For exports
#include "sv_VTK.h"
#include "sv_PolyData.h"

SV_EXPORT_SYSGEOM int sys_geom_IntegrateSurface( cvPolyData *src, int tensorType, double *nrm, double *q );

SV_EXPORT_SYSGEOM int sys_geom_IntegrateSurface2( vtkPolyData *pd, int tensorType, double *q, double *area );

SV_EXPORT_SYSGEOM int sys_geom_IntegrateSurface2( cvPolyData *src, int tensorType, double *q, double *area );

SV_EXPORT_SYSGEOM int sys_geom_IntegrateScalarSurf ( cvPolyData *src, double *q );

SV_EXPORT_SYSGEOM int sys_geom_IntegrateScalarThresh ( cvPolyData *src, double wssthresh, double *q, double *a );

SV_EXPORT_SYSGEOM int sys_geom_IntegrateEnergy ( cvPolyData *src, double rho, double *nrm, double *energy );

#endif
