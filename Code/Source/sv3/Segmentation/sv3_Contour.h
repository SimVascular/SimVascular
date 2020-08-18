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

#ifndef SV3_CONTOUR_H
#define SV3_CONTOUR_H

#include "SimVascular.h"

#include <sv3SegmentationExports.h>

#include "sv_RepositoryData.h"
#include "sv_FactoryRegistrar.h"
#include "sv3_PathElement.h"

#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkImageData.h"
#include "vtkPlane.h"

// somehow GetClassName is getting set to GetClassNameA on Windows
#ifdef GetClassName
#undef GetClassName
#endif

enum cKernelType {
    cKERNEL_INVALID,
    cKERNEL_LEVELSET,
    cKERNEL_THRESHOLD,
    cKERNEL_CIRCLE,
    cKERNEL_POLYGON,
    cKERNEL_SPLINEPOLYGON,
    cKERNEL_ELLIPSE,
    cKERNEL_CONTOUR
};

namespace sv3{
class SV_EXPORT_SEGMENTATION Contour : public cvRepositoryData
{

public:

    struct svLSParam
    {
        double ctrx;
        double ctry;
        double ctrz;
        double radius;

        double sigmaFeat1;
        double sigmaAdv1;
        double kc;
        double expFactorRising;
        double expFactorFalling;
        int maxIter1;
        double maxErr1;

        double sigmaFeat2;
        double sigmaAdv2;
        double kupp;
        double klow;
        int maxIter2;
        double maxErr2;

        svLSParam()
            : ctrx(0.0)
            , ctry(0.0)
            , ctrz(0.0)
            , radius(0.3)
            , sigmaFeat1(2.5)
            , sigmaAdv1(0.0)
            , kc(0.6)
            , expFactorRising(0.25)
            , expFactorFalling(0.5)
            , maxIter1(2000)
            , maxErr1(0.001)
            , sigmaFeat2(1.5)
            , sigmaAdv2(0.0)
            , kupp(0.8)
            , klow(0.09)
            , maxIter2(1000)
            , maxErr2(0.0005)
        {
        }

    };

    cKernelType GetKernel() const {return contour_kernel_;}
    void SetKernel(cKernelType ktype) {contour_kernel_ = ktype;}
    
    static cKernelType gCurrentKernel;
    
    enum SubdivisionType {CONSTANT_TOTAL_NUMBER, CONSTANT_SUBDIVISION_NUMBER,CONSTANT_SPACING};

    static const int INVALID_INDEX=-2;

    Contour();

    Contour(const Contour &other);

    virtual ~Contour();

    virtual Contour* Clone(){return NULL;};

    virtual std::string GetClassName();

    std::string GetType();

    void SetType(std::string type);

    std::string GetMethod();

    void SetMethod(std::string method);

    PathElement::PathPoint GetPathPoint();

    void SetPathPoint(PathElement::PathPoint pathPoint);

    int GetPathPosID();

    std::array<double, 3> GetPathPosPoint();

    int GetContourID();

    void SetContourID(int contourID);

    bool IsClosed();

    void SetClosed(bool closed=true);

    bool IsFinished();

    void SetFinished(bool finished=true);

    int GetSubdivisionNumber();

    void SetSubdivisionNumber(int number);

    SubdivisionType GetSubdivisionType();

    void SetSubdivisionType(SubdivisionType subdivType);

    double GetSubdivisionSpacing();

    void SetSubdivisionSpacing(double spacing);

    void SetPlaneGeometry(vtkPlane *planeGeometry);

    vtkPlane* GetPlaneGeometry();

    int GetControlPointNumber();

    int GetMinControlPointNumber();
    int GetMaxControlPointNumber();

    void SetMinControlPointNumber(int number);
    void SetMaxControlPointNumber(int number);

    std::array<double,3> GetControlPoint(int index);

    void InsertControlPoint(int index, std::array<double,3> point);

    void RemoveControlPoint(int index);

    virtual void SetControlPoint(int index, std::array<double,3> point);

    std::vector<std::array<double,3> > GetControlPoints();

    void SetControlPointSelectedIndex(int index);

    void DeselectControlPoint();

    int GetControlPointSelectedIndex();

    void ClearControlPoints();

    void PlaceContour(std::array<double,3> point);

    virtual void PlaceControlPoints(std::array<double,3> point);

    void SetControlPoints(std::vector<std::array<double,3> > controlPoints, bool updateContour = true);
    
    virtual void SetControlPointByRadius(double radius, double* point){return;};

    bool IsControlPointRemovable(int index);

    void SetPreviewControlPoint(std::array<double,3> point );

    std::array<double,3> GetPreviewControlPoint();

    //for contour points
    //=================================

    virtual void CreateContourPoints(){return;};
    
    void CreateContour();

    void SetContourPoints(std::vector<std::array<double,3> > contourPoints, bool update = true);

    void ControlPointsChanged();

    int GetContourPointNumber();

    std::array<double,3> GetContourPoint(int index);
    
    std::vector<std::array<double,3> > GetContourPoints();

    void ClearContourPoints();

    vtkSmartPointer<vtkPolyData> CreateVtkPolyDataFromContour(bool includingAllLines = true);

    virtual void ContourPointsChanged();

    virtual void CreateCenterScalingPoints();
    virtual void AssignCenterScalingPoints();

    std::array<double,3> GetCenterPoint();

    virtual Contour* CreateSmoothedContour(int fourierNumber = 12 );

    //for all data
    //===================================

    virtual int SearchControlPointByContourPoint( int contourPointIndex );

    void Shift(std::array<double,3> dirVec);

    void Scale(double factor, std::array<double,3> referencePoint);

    void Scale(double factor);

    void Scale(std::array<double,3> referencePoint, std::array<double,3> oldPoint, std::array<double,3> newPoint);

    void CalculateBoundingBox(double *bounds);

    bool IsOnPlane(const vtkPlane * planeGeometry, double precisionFactor=0.1);

    vtkImageData* GetVtkImageSlice();

    void SetVtkImageSlice(vtkImageData* slice);

    int GetTagIndex() {return m_TagIndex;}
    void SetTagIndex(int idx) {m_TagIndex=idx;}

    double GetArea();

    double GetPerimeter();
    
    //for setting parameters:
    //==========================================
    
    virtual void SetLevelSetParas(svLSParam* paras){return;};
    
    virtual svLSParam* GetLevelSetParas(){return NULL;};
    
    virtual void SetThresholdValue(double thresholdValue){return;}
    
    virtual double GetThresholdValue(){return 0.;}
    
  protected:

    int m_ContourID;

    PathElement::PathPoint m_PathPoint;

    std::string m_Method;

    std::string m_Type;

    bool m_Closed;

    bool m_Finished;
    
    vtkPlane * m_vtkPlaneGeometry;

    std::array<double,3> m_CenterPoint;

    std::array<double,3> m_ScalingPoint;

    int m_ControlPointSelectedIndex;

    int m_MinControlPointNumber;
    
    int m_MaxControlPointNumber;

    std::vector<std::array<double,3> > m_ControlPoints;

    std::vector<std::array<double,3> > m_ContourPoints;

    int m_ControlPointNonRemovableIndices[5];

    bool m_InitiallyPlaced;

    vtkImageData* m_VtkImageSlice;

    int m_SubdivisionNumber;

    SubdivisionType m_SubdivisionType;

    double m_SubdivisionSpacing;

    int m_TagIndex;
    
    cKernelType contour_kernel_;

  };

}
#endif // SV3_CONTOUR_H
