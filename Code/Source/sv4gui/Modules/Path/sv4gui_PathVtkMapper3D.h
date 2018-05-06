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

#ifndef SV4GUI_PATHVTKMAPPER3D_H
#define SV4GUI_PATHVTKMAPPER3D_H

#include "SimVascular.h"

#include <sv4guiModulePathExports.h>

#include "sv4gui_Path.h"

#include "mitkVtkMapper.h"
#include "mitkBaseRenderer.h"
#include <vtkSmartPointer.h>

class vtkActor;
class vtkPropAssembly;
class vtkAppendPolyData;
class vtkPolyData;
class vtkTubeFilter;
class vtkPolyDataMapper;

class SV4GUIMODULEPATH_EXPORT sv4guiPathVtkMapper3D : public mitk::VtkMapper
{
public:
    mitkClassMacro(sv4guiPathVtkMapper3D, mitk::VtkMapper);

    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    virtual const sv4guiPath* GetInput();

    virtual vtkProp* GetVtkProp(mitk::BaseRenderer* renderer) override;
    virtual void UpdateVtkTransform(mitk::BaseRenderer* renderer) override;

    static void SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer = NULL, bool overwrite = false);

    void ReleaseGraphicsResources(mitk::BaseRenderer* renderer) override;

    mitk::LocalStorageHandler<BaseLocalStorage> m_LSH;

protected:
    sv4guiPathVtkMapper3D();

    virtual ~sv4guiPathVtkMapper3D();

    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;
    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;
    virtual void ApplyAllProperties(mitk::BaseRenderer* renderer);
    virtual void CreateSpline();
    virtual void CreateVTKRenderObjects();

    vtkSmartPointer<vtkAppendPolyData> m_vtkSelectedPoints;
    vtkSmartPointer<vtkAppendPolyData> m_vtkUnselectedPoints;
    vtkSmartPointer<vtkAppendPolyData> m_vtkSplinePoints;

    vtkSmartPointer<vtkPolyDataMapper> m_VtkSelectedPolyDataMapper;
    vtkSmartPointer<vtkPolyDataMapper> m_VtkUnselectedPolyDataMapper;
    vtkSmartPointer<vtkPolyDataMapper> m_VtkSplinePointsPolyDataMapper;
    vtkSmartPointer<vtkPolyDataMapper> m_VtkSplinePolyDataMapper;

    vtkSmartPointer<vtkActor> m_SelectedActor;
    vtkSmartPointer<vtkActor> m_UnselectedActor;
    vtkSmartPointer<vtkActor> m_SplinePointsActor;
    vtkSmartPointer<vtkActor> m_SplineActor;

    vtkSmartPointer<vtkPropAssembly> m_PropAssembly;

    //variables to check if an update of the vtk objects is needed
    mitk::ScalarType m_PointSize;          // "point size" property
    mitk::ScalarType m_SplinePointSize;    // "spline point size" property
    mitk::ScalarType m_SplineRadius;       // "spline size" property

};


#endif // SV4GUI_PATHVTKMAPPER3D_H
