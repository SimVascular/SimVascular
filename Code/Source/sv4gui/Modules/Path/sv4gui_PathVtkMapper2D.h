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

#ifndef SV4GUI_PATHVTKMAPPER2D_H
#define SV4GUI_PATHVTKMAPPER2D_H

#include "SimVascular.h"

#include <sv4guiModulePathExports.h>

#include "sv4gui_Path.h"

#include "mitkVtkMapper.h"
#include "mitkBaseRenderer.h"
#include "mitkLocalStorageHandler.h"

//VTK
#include <vtkSmartPointer.h>
class vtkActor;
class vtkPropAssembly;
class vtkPolyData;
class vtkPolyDataMapper;
class vtkGlyphSource2D;
class vtkGlyph3D;
class vtkFloatArray;
class vtkCellArray;

class SV4GUIMODULEPATH_EXPORT sv4guiPathVtkMapper2D : public mitk::VtkMapper
{
public:
    mitkClassMacro(sv4guiPathVtkMapper2D, mitk::VtkMapper);

    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual const sv4guiPath* GetInput() const;

    static bool makePerpendicularVector2D(const mitk::Vector2D& in, mitk::Vector2D& out);

    virtual vtkProp* GetVtkProp(mitk::BaseRenderer* renderer) override;

    static void SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer = NULL, bool overwrite = false);

    class SV4GUIMODULEPATH_EXPORT LocalStorage : public mitk::Mapper::BaseLocalStorage
    {

    public:

        LocalStorage();

        ~LocalStorage();

        // points
        vtkSmartPointer<vtkPoints> m_UnselectedPoints;
        vtkSmartPointer<vtkPoints> m_SelectedPoints;
        //    vtkSmartPointer<vtkPoints> m_ContourPoints;
        vtkSmartPointer<vtkPoints> m_SplinePoints;
        vtkSmartPointer<vtkPoints> m_SplineConnectingPoints;

        // scales
        vtkSmartPointer<vtkFloatArray> m_UnselectedScales;
        vtkSmartPointer<vtkFloatArray> m_SelectedScales;
        vtkSmartPointer<vtkFloatArray> m_SplinePointsScales;

        // distances
        //    vtkSmartPointer<vtkFloatArray> m_DistancesBetweenPoints;

        // lines
        //    vtkSmartPointer<vtkCellArray> m_ContourLines;
        vtkSmartPointer<vtkCellArray> m_SplineConnectingLines;

        // glyph source (provides different shapes for the points)
        vtkSmartPointer<vtkGlyphSource2D> m_UnselectedGlyphSource2D;
        vtkSmartPointer<vtkGlyphSource2D> m_SelectedGlyphSource2D;
        vtkSmartPointer<vtkGlyphSource2D> m_SplineGlyphSource2D;

        // glyph
        vtkSmartPointer<vtkGlyph3D> m_UnselectedGlyph3D;
        vtkSmartPointer<vtkGlyph3D> m_SelectedGlyph3D;
        vtkSmartPointer<vtkGlyph3D> m_SplineGlyph3D;

        // polydata
        vtkSmartPointer<vtkPolyData> m_VtkUnselectedPointsPolyData;
        vtkSmartPointer<vtkPolyData> m_VtkSelectedPointsPolyData;
        //    vtkSmartPointer<vtkPolyData> m_VtkContourPolyData;
        vtkSmartPointer<vtkPolyData> m_VtkSplinePointsPolyData;
        vtkSmartPointer<vtkPolyData> m_VtkSplineConnectingLinesPolyData;

        // actor
        vtkSmartPointer<vtkActor> m_UnselectedActor;
        vtkSmartPointer<vtkActor> m_SelectedActor;
        //    vtkSmartPointer<vtkActor> m_ContourActor;
        vtkSmartPointer<vtkActor> m_SplinePointsActor;
        vtkSmartPointer<vtkActor> m_SplineConnectingLinesActor;
        //    vtkSmartPointer<vtkTextActor> m_VtkTextActor;

        //    std::vector < vtkSmartPointer<vtkTextActor> > m_VtkTextLabelActors;
        //    std::vector < vtkSmartPointer<vtkTextActor> > m_VtkTextDistanceActors;
        //    std::vector < vtkSmartPointer<vtkTextActor> > m_VtkTextAngleActors;

        // mappers
        vtkSmartPointer<vtkPolyDataMapper> m_VtkUnselectedPolyDataMapper;
        vtkSmartPointer<vtkPolyDataMapper> m_VtkSelectedPolyDataMapper;
        //    vtkSmartPointer<vtkPolyDataMapper> m_VtkContourPolyDataMapper;
        vtkSmartPointer<vtkPolyDataMapper> m_VtkSplinePointsPolyDataMapper;
        vtkSmartPointer<vtkPolyDataMapper> m_VtkSplineConnectingLinesPolyDataMapper;

        // propassembly
        vtkSmartPointer<vtkPropAssembly> m_PropAssembly;

    };

    mitk::LocalStorageHandler<LocalStorage> m_LSH;

protected:

    sv4guiPathVtkMapper2D();

    virtual ~sv4guiPathVtkMapper2D();

    virtual void CreateVTKRenderObjects(mitk::BaseRenderer* renderer);

    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;

    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;

    // member variables holding the current value of the properties used in this mapper
    //  bool m_ShowContour;             // "show contour" property
    bool m_ShowSpline;             // "show spline" property
    bool m_ShowSplinePoints;             // "show spline points" property
    //  bool m_CloseContour;            // "close contour" property
    bool m_ShowPoints;              // "show points" property
    //  bool m_ShowDistances;           // "show distances" property
    //  int m_DistancesDecimalDigits;   // "distance decimal digits" property
    //  bool m_ShowAngles;              // "show angles" property
    //  bool m_ShowDistantLines;        // "show distant lines" property
    int m_LineWidth;                // "line width" property
    int m_PointLineWidth;           // "point line width" property
    float m_Point2DSize;              // "point 2D size" property, in display units
    float m_SplinePoint2DSize;        // "spline point 2D size" property, in display units
    //  int m_IDShapeProperty;          // ID for mitkPointSetShape Enumeration Property "Pointset.2D.shape"
    //  bool m_FillShape;               // "Pointset.2D.fill shape" property
    float m_DistanceToPlane;        // "point 2D distance to plane" property


};

#endif // SV4GUI_PATHVTKMAPPER2D_H
