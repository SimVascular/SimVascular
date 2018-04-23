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

#include "sv4gui_LoftingUtils.h"

#include <berryIPreferences.h>
#include <berryIPreferencesService.h>
#include <berryPlatform.h>

void sv4guiLoftingUtils::SetPreferencedValues(svLoftingParam* param)
{
    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    Q_ASSERT(prefService);
    berry::IPreferences::Pointer preferences = prefService->GetSystemPreferences()->Node("/org.sv.views.lofting");
    if(preferences.IsNull())
        return;

    if(param==NULL)
        return;

     param->method= preferences->Get("Lofting Method", QString::fromStdString(param->method)).toStdString();

     param->uDegree= preferences->GetInt("NURBS Lofting U Degree", param->uDegree);
     param->vDegree= preferences->GetInt("NURBS Lofting V Degree", param->vDegree);
     param->uKnotSpanType= preferences->Get("NURBS Lofting U Knot Span Type", QString::fromStdString(param->uKnotSpanType)).toStdString();
     param->vKnotSpanType= preferences->Get("NURBS Lofting V Knot Span Type", QString::fromStdString(param->vKnotSpanType)).toStdString();
     param->uParametricSpanType= preferences->Get("NURBS Lofting U Parametric Span Type", QString::fromStdString(param->uParametricSpanType)).toStdString();
     param->vParametricSpanType= preferences->Get("NURBS Lofting V Parametric Span Type", QString::fromStdString(param->vParametricSpanType)).toStdString();

     param->numOutPtsInSegs= preferences->GetInt("Spline Sampling", param->numOutPtsInSegs);
     param->samplePerSegment= preferences->GetInt("Spline Point Number Per Segment", param->samplePerSegment);
     param->useLinearSampleAlongLength= preferences->GetInt("Spline Use Linear Sample", param->useLinearSampleAlongLength);
     param->linearMuliplier= preferences->GetInt("Spline Linear Sample Factor", param->linearMuliplier);
     param->useFFT= preferences->GetInt("Spline Use FFT", param->useFFT);
     param->numModes= preferences->GetInt("Spline FFT Mode Number", param->numModes);
}
