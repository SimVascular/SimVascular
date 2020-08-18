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

#ifndef SV4GUI_CONTOUR_H
#define SV4GUI_CONTOUR_H

#include "SimVascular.h"

#include <sv4guiModuleSegmentationExports.h>

#include "sv4gui_PathElement.h"
#include "sv3_Contour.h"

#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkImageData.h"

#include "mitkBaseData.h"
#include "mitkPoint.h"
#include "mitkPlaneGeometry.h"

// somehow GetClassName is getting set to GetClassNameA on Windows
#ifdef GetClassName
#undef GetClassName
#endif
using sv3::Contour;
class SV4GUIMODULESEGMENTATION_EXPORT sv4guiContour : public Contour
{

public:

    sv4guiContour();

    sv4guiContour(const sv4guiContour &other);

    virtual ~sv4guiContour();

    virtual sv4guiContour* Clone();

    void CopyContourData(sv3::Contour* contour);

    sv4guiPathElement::sv4guiPathPoint GetPathPoint();

    void SetPathPoint(sv4guiPathElement::sv4guiPathPoint pathPoint);

    mitk::Point3D GetPathPosPoint();

    bool IsSelected();

    void SetSelected(bool selected=true);

    bool IsHovering();

    void SetHovering(bool hovering=true);

    bool IsPlaced();

    void SetPlaced(bool placed=true);

    bool IsExtendable();

    void SetExtendable(bool extendable=true);

    bool IsInitiallyPlaced(){return m_InitiallyPlaced;}

    void SetInitiallyPlaced(bool placed){m_InitiallyPlaced=placed;}

    void SetPlaneGeometry(mitk::PlaneGeometry* planeGeometry);

    mitk::PlaneGeometry* GetPlaneGeometry();

    mitk::Point3D GetControlPoint(int index);
    
    void InsertControlPoint(int index, mitk::Point3D point);

    virtual void SetControlPoint(int index, mitk::Point3D point);

    void PlaceContour(mitk::Point3D point);

    virtual void PlaceControlPoints(mitk::Point3D point);

    void SetControlPoints(std::vector<mitk::Point3D> controlPoints, bool updateContour = true);

    void SetPreviewControlPoint(mitk::Point3D point );
    
    void HidePreviewControlPoint();
    
    bool IsPreviewControlPointVisible();

    mitk::Point3D GetPreviewControlPoint();

    //for contour points
    //=================================

    virtual void CreateContourPoints(){}

    void SetContourPoints(std::vector<mitk::Point3D> contourPoints, bool update = true);
    
    void SetContourPoints(std::vector<std::array<double,3> > contourPoints, bool update=true);

    mitk::Point3D GetContourPoint(int index);

    mitk::Point3D GetCenterPoint();

    virtual sv4guiContour* CreateSmoothedContour(int fourierNumber = 12 );

    //for all data
    //===================================

    void Shift(mitk::Vector3D dirVec);

    void Scale(double factor, mitk::Point3D referencePoint);

    void Scale(double factor);

    void Scale(mitk::Point3D referencePoint, mitk::Point3D oldPoint, mitk::Point3D newPoint);

    bool IsOnPlane(const mitk::PlaneGeometry* planeGeometry, double precisionFactor=0.1);

  protected:

    bool m_Selected;

    bool m_Hovering;

    bool m_Placed;

    bool m_Extendable;

    mitk::PlaneGeometry::Pointer m_PlaneGeometry;

    mitk::Point3D m_PreviewControlPoint;

    bool m_PreviewControlPointVisible;


  };


#endif // SV4GUI_CONTOUR_H
