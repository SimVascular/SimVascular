#include "svLoftingUtils.h"

#include <berryIPreferences.h>
#include <berryIPreferencesService.h>
#include <berryPlatform.h>

void svLoftingUtils::SetPreferencedValues(svLoftingParam* param)
{
    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    Q_ASSERT(prefService);
    berry::IPreferences::Pointer preferences = prefService->GetSystemPreferences()->Node("/org.sv.views.lofting");
    if(preferences.IsNull())
        return;

    if(param==NULL)
        return;

     param->method= preferences->GetInt("Lofting Method", param->method);

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
