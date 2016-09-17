#ifndef SVVTKPARAMETRICSPLINE_H
#define SVVTKPARAMETRICSPLINE_H

#include "SimVascular.h"

#include "svCommonExports.h"

#include "vtkParametricSpline.h"

class SVCOMMON_EXPORT svVtkParametricSpline : public vtkParametricSpline
{
public:

    svVtkParametricSpline();

    ~svVtkParametricSpline();

    void Evaluate(double t, double Pt[3]);

    void EvaluateByLengthFactor(double t, double Pt[3]);


};
#endif // SVVTKPARAMETRICSPLINE_H
