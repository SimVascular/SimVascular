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

#ifndef SV4GUI_IMAGESEED_MAPPER2D_H
#define SV4GUI_IMAGESEED_MAPPER2D_H

#include "SimVascular.h"

//#include <sv4guiModulePathExports.h>

//#include "sv4gui_Path.h"

#include "mitkVtkMapper.h"
#include "mitkBaseRenderer.h"
#include "mitkLocalStorageHandler.h"

#include <vtkSmartPointer.h>

class vtkActor;
class vtkPropAssembly;
class vtkPolyData;
class vtkPolyDataMapper;
class vtkGlyphSource2D;
class vtkGlyph3D;
class vtkFloatArray;
class vtkCellArray;

//-------------------------
// sv4guiImageSeedMapper2D
//-------------------------
// This class is used to display seeds in 2D windows. 
//
// It is implemented as a VTK-based mapper for surfaces.
//
class sv4guiImageSeedMapper2D : public mitk::VtkMapper
{
  public:
    mitkClassMacro(sv4guiImageSeedMapper2D, mitk::VtkMapper);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    class LocalStorage : public mitk::Mapper::BaseLocalStorage
    {
      public:
        LocalStorage();
        ~LocalStorage();

        vtkSmartPointer<vtkPoints> m_SeedPoints;
        vtkSmartPointer<vtkFloatArray> m_SeedPointsScale;

        vtkSmartPointer<vtkGlyph3D> m_SeedPointsGlyph3D;
        vtkSmartPointer<vtkPolyData> m_SeedPointsPolyData;

        vtkSmartPointer<vtkActor> m_SeedPointsActor;

        vtkSmartPointer<vtkPolyDataMapper> m_SeedPointsPolyDataMapper;

        // Object that groups graphics properties into a tree-like hierarchy.
        vtkSmartPointer<vtkPropAssembly> m_PropAssembly;
    };

    virtual vtkProp* GetVtkProp(mitk::BaseRenderer* renderer) override;

    mitk::LocalStorageHandler<LocalStorage> m_LSH;

    //static void SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer = NULL, bool overwrite = false);

    double m_SeedRadius = 0.5;

  protected:
    sv4guiImageSeedMapper2D();
    virtual ~sv4guiImageSeedMapper2D();

    virtual void CreateVTKRenderObjects(mitk::BaseRenderer* renderer);
    virtual void GenerateDataForRenderer(mitk::BaseRenderer* renderer) override;
    virtual void ResetMapper( mitk::BaseRenderer* renderer ) override;

    // "point 2D size" property, in display units
    float m_SeedPoint2DSize;   

    // "point 2D distance to plane" property
    float m_DistanceToPlane;        
};

#endif // SV4GUI_PATHVTKMAPPER2D_H
