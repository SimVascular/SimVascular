#include "svVtkParametricSpline.h"
#include "vtkSmartPointer.h"
#include "vtkSpline.h"
#include "vtkPoints.h"

svVtkParametricSpline::svVtkParametricSpline()
{
}

svVtkParametricSpline::~svVtkParametricSpline()
{

}

void svVtkParametricSpline::Evaluate(double t, double Pt[3])
{
    if ( this->InitializeTime < this->GetMTime () )
    {
        if ( ! this->Initialize() )
        {
            return;
        }
    }

    if(t<0) t=0;

    if ( this->Closed )
    {
        if(t > Points->GetNumberOfPoints()) t=Points->GetNumberOfPoints();
    }
    else
    {
        if(t > Points->GetNumberOfPoints()-1) t=Points->GetNumberOfPoints()-1;
    }

    if (this->Length == 0 && this->Points && this->Points->GetNumberOfPoints() > 0)
    {
        this->Points->GetPoint(0, Pt);
        return;
    }

    Pt[0] = this->XSpline->Evaluate(t);
    Pt[1] = this->YSpline->Evaluate(t);
    Pt[2] = this->ZSpline->Evaluate(t);
}

void svVtkParametricSpline::EvaluateByLengthFactor(double t, double Pt[3])
{
    double U[3]={0};
    U[0]=t;

    vtkParametricSpline::Evaluate(U, Pt, NULL);
}
