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

#ifndef SV4GUI_CONTOURMODELVTKMAPPER2D_H
#define SV4GUI_CONTOURMODELVTKMAPPER2D_H

#include "SimVascular.h"

#include <sv4guiModuleSegmentationExports.h>

#include "sv4gui_ContourModel.h"
#include "sv4gui_Contour.h"

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

class SV4GUIMODULESEGMENTATION_EXPORT sv4guiContourModelVtkMapper2D : public mitk::VtkMapper
{
public:

    mitkClassMacro(sv4guiContourModelVtkMapper2D, mitk::VtkMapper);

    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual vtkProp* GetVtkProp(mitk::BaseRenderer* renderer) override;

    static void SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer = NULL, bool overwrite = false);

    void ReleaseGraphicsResources(mitk::BaseRenderer* renderer) override;

    class LocalStorage : public mitk::Mapper::BaseLocalStorage
    {

    public:

        LocalStorage();

        ~LocalStorage();

        // points
        vtkSmartPointer<vtkPoints> m_UnselectedPoints;
        vtkSmartPointer<vtkPoints> m_SelectedPoints;

        // scales
        vtkSmartPointer<vtkFloatArray> m_UnselectedScales;
        vtkSmartPointer<vtkFloatArray> m_SelectedScales;

        // glyph source (provides different shapes for the points)
        vtkSmartPointer<vtkGlyphSource2D> m_UnselectedGlyphSource2D;
        vtkSmartPointer<vtkGlyphSource2D> m_SelectedGlyphSource2D;

        // glyph
        vtkSmartPointer<vtkGlyph3D> m_UnselectedGlyph3D;
        vtkSmartPointer<vtkGlyph3D> m_SelectedGlyph3D;

        // polydata
        vtkSmartPointer<vtkPolyData> m_VtkUnselectedPointsPolyData;
        vtkSmartPointer<vtkPolyData> m_VtkSelectedPointsPolyData;
        vtkSmartPointer<vtkPolyData> m_VtkContourPolyData;

        // actor
        vtkSmartPointer<vtkActor> m_UnselectedActor;
        vtkSmartPointer<vtkActor> m_SelectedActor;
        vtkSmartPointer<vtkActor> m_ContourActor;

        // mappers
        vtkSmartPointer<vtkPolyDataMapper> m_VtkUnselectedPolyDataMapper;
        vtkSmartPointer<vtkPolyDataMapper> m_VtkSelectedPolyDataMapper;
        vtkSmartPointer<vtkPolyDataMapper> m_VtkContourPolyDataMapper;

        // propassembly
        vtkSmartPointer<vtkPropAssembly> m_PropAssembly;

    };

    mitk::LocalStorageHandler<LocalStorage> m_LSH;

protected:

    sv4guiContourModelVtkMapper2D();

    virtual ~sv4guiContourModelVtkMapper2D();

    virtual void CreateVTKRenderObjects(mitk::BaseRenderer* renderer);

    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;

    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;

    virtual void FindContourOnCurrentSlice(mitk::BaseRenderer* renderer, unsigned int t = 0) ;

    sv4guiContour* m_Contour;//available contour on the renderer plane

    // member variables holding the current value of the properties used in this mapper
    //  bool m_ShowContour;             // "show contour" property
    bool m_ShowPoints;              // "show points" property
    int m_LineWidth;                // "line width" property
    int m_PointLineWidth;           // "point line width" property
    float m_Point2DSize;              // "point 2D size" property
    float m_DistanceToPlane;        // "point 2D distance to plane" property


};

#endif // SV4GUI_CONTOURMODELVTKMAPPER2D_H
